import datetime
import enum
import json
import os
import traceback


# This file contains the facilities to create telemetry messages. It has been copied (with slight modification) from the
# python extractor (ql/python/extractor/semmle/logging.py and ql/python/extractor/semmle/worker.py)

class StructuredLogObject(object):
    """
    Base class for CodeQL telemetry message format

    see https://github.com/github/code-scanning/blob/main/docs/adrs/0035-diagnostics.md#codeql-diagnostic-message-format
    """

    def to_dict(self):
        # Discard any entries with a value of `None`
        def f(v):
            if isinstance(v, StructuredLogObject):
                return v.to_dict()
            return v

        return {k: f(v) for k, v in self.__dict__.items() if v is not None}


class Severity(StructuredLogObject, enum.Enum):
    ERROR = "error"
    WARNING = "warning"
    NOTE = "note"

    def to_dict(self):
        return self.value


class Source(StructuredLogObject):
    def __init__(self, id, name, extractorName="cpp/bmn"):
        self.id = id
        self.name = name
        self.extractorName = extractorName

    def extractorName(self, extractorName):
        self.extractorName = extractorName
        return self


class Visibility(StructuredLogObject):
    def __init__(self, statusPage=False, cliSummaryTable=False, telemetry=False):
        self.statusPage = statusPage
        self.cliSummaryTable = cliSummaryTable
        self.telemetry = telemetry

    def statusPage(self, statusPage):
        self.statusPage = statusPage
        return self

    def cliSummaryTable(self, cliSummaryTable):
        self.cliSummaryTable = cliSummaryTable
        return self

    def telemetry(self, telemetry):
        self.telemetry = telemetry
        return self


class Location(StructuredLogObject):
    def __init__(self, file=None, startLine=None, startColumn=None, endLine=None, endColumn=None):
        self.file = file
        self.startLine = startLine
        self.startColumn = startColumn

        # If you set startline/startColumn you MUST also set endLine/endColumn, so we
        # ensure they are also set.
        self.endLine = endLine
        if endLine is None and startLine is not None:
            self.endLine = startLine

        self.endColumn = endColumn
        if endColumn is None and startColumn is not None:
            self.endColumn = startColumn

    def file(self, file):
        self.file = file
        return self

    def startLine(self, startLine):
        self.startLine = startLine
        return self

    def startColumn(self, startColumn):
        self.startColumn = startColumn
        return self

    def endLine(self, endLine):
        self.endLine = endLine
        return self

    def endColumn(self, endColumn):
        self.endColumn = endColumn
        return self


class DiagnosticMessage(StructuredLogObject):
    def __init__(self, source, severity=Severity.WARNING, location=None, markdownMessage=None, plaintextMessage=None,
                 helpLinks=None, visibility=None, attributes=None, timestamp=None):
        self.timestamp = timestamp or datetime.datetime.now().isoformat()
        self.source = source
        self.severity = severity
        self.location = location
        self.markdownMessage = markdownMessage
        self.plaintextMessage = plaintextMessage
        self.helpLinks = helpLinks
        if visibility is None:
            visibility = Visibility()
        self.visibility = visibility
        self.attributes = attributes

    def with_severity(self, severity):
        self.severity = severity
        return self

    def with_location(self, location):
        self.location = location
        return self

    def markdown(self, message):
        self.markdownMessage = message
        return self

    def text(self, message):
        self.plaintextMessage = message
        return self

    def help_link(self, link):
        if self.helpLinks is None:
            self.helpLinks = []
        self.helpLinks.append(link)
        return self

    def cli_summary_table(self):
        self.visibility.cliSummaryTable = True
        return self

    def status_page(self):
        self.visibility.statusPage = True
        return self

    def telemetry(self):
        self.visibility.telemetry = True
        return self

    def attribute(self, key, value):
        if self.attributes is None:
            self.attributes = {}
        self.attributes[key] = value
        return self

    def attributes_from(self, attributes):
        if self.attributes is None:
            self.attributes = {}
        self.attributes.update(attributes)
        return self

    def with_timestamp(self, timestamp):
        self.timestamp = timestamp
        return self


def select_traceback_lines(lines, limit_start=30, limit_end=12):
    '''Select a subset of traceback lines to be displayed, cutting out the middle part of the
    traceback if the length exceeds `limit_start + limit_end`.
    This is intended to avoid displaying too many lines of tracebacks
    that are not relevant to the user.'''
    lines = lines.splitlines()
    num_lines = len(lines)
    limit = limit_start + limit_end
    if num_lines <= limit:
        yield from lines
    else:
        yield from lines[:limit_start]
        yield "... {} lines skipped".format(num_lines - limit)
        yield from lines[-limit_end:]


def trim_traceback(lines):
    trimmed = []
    for line in select_traceback_lines(lines):
        shortline = line.strip()
        try:
            if shortline.startswith("File"):
                shortline = '"semmle' + shortline.split("semmle")[-1]
            elif shortline.startswith("..."):
                pass
            else:
                continue
        except Exception:
            # Formatting error, just emit line as-is.
            pass
        trimmed.append(shortline)
    return trimmed


def get_stack_trace_lines():
    """Creates a stack trace for inclusion into the `attributes` part of a diagnostic message.
    Limits the size of the stack trace to 5000 characters, so as to not make the SARIF file overly big.
    """
    lines = trim_traceback(traceback.format_exc())
    trace_length = 0
    for i, line in enumerate(lines):
        trace_length += len(line)
        if trace_length > 5000:
            return lines[:i]
    return lines


def standalone_error_message(message_name, exception):
    """
    Produce a telemetry message for a standalone extraction error
    :param message_name: The name of the telemetry message (the id is the name in lower case with spaces replaced by dashes)
    :param exception: The exception that was raised
    :return: A DiagnosticMessage telemetry error object
    """
    message_id = message_name.lower().replace(" ", "-")
    return (DiagnosticMessage(Source(f"cpp/bmn/{message_id}", "Standalone extraction error", "cpp/bmn"),
                              Severity.ERROR)
            .text("Internal standalone extraction error")
            .attribute("traceback", get_stack_trace_lines())
            .attribute("args", exception.args)
            .telemetry()
            )


def telemetry_message(message_name, attributes_dict):
    """
    Produce a telemetry message for standalone extraction information
    :param message_name: The name of the telemetry message (the id is the name in lower case with spaces replaced by dashes)
    :param attributes_dict: The attribute dictionary to be included in the telemetry
    :return: A DiagnosticMessage telemetry object
    """
    message_id = message_name.lower().replace(" ", "-")
    error = (
        DiagnosticMessage(
            Source(f"cpp/bmn/{message_id}", message_name, "cpp/bmn"),
            Severity.NOTE)
        .attributes_from(attributes_dict)
        .telemetry()
    )
    return error


# Class to write diagnostics messages to a file
class DiagnosticsWriter(object):
    # Static variable to hold the writer instance
    instance = None

    def __init__(self, proc_id):
        self.proc_id = proc_id

    def write(self, message):
        """
        Write a telemetry message to the diagnostics file.
        :param message: The message to be written
        """
        dir = os.environ.get("CODEQL_EXTRACTOR_CPP_DIAGNOSTIC_DIR")
        if dir:
            with open(os.path.join(dir, "standalone-extraction-%d.jsonl" % self.proc_id), "a") as output_file:
                output_file.write(json.dumps(message.to_dict()) + "\n")

    @staticmethod
    def create_output_dir():
        """
        Create the output directory for diagnostics messages if it does not exist.
        """
        dir = os.environ.get("CODEQL_EXTRACTOR_CPP_DIAGNOSTIC_DIR")
        if dir:
            os.makedirs(os.environ["CODEQL_EXTRACTOR_CPP_DIAGNOSTIC_DIR"], exist_ok=True)

    @staticmethod
    def initialize():
        """
        Initialize the DiagnosticsWriter instance.
        """
        DiagnosticsWriter.create_output_dir()
        if DiagnosticsWriter.instance is None:
            DiagnosticsWriter.instance = DiagnosticsWriter(os.getpid())
