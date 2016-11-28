/// <summary>
/// Handles parsing and execution of console commands, as well as collecting log output.
/// Copyright (c) 2014-2015 Eliot Lash
/// </summary>
using System.Collections.Generic;

public delegate void CommandHandler(string[] args);

public class ConsoleController
{

    #region Event declarations
    // Used to communicate with ConsoleView
    public delegate void LogChangedHandler(string[] log);
    public event LogChangedHandler LogChanged;

    public delegate void VisibilityChangedHandler(bool visible);
    public event VisibilityChangedHandler VisibilityChanged;
    #endregion

    public ConsoleController(int lineCount)
    {
        ScrollbackSize = lineCount;
        _scrollback = new Queue<string>(lineCount);
    }

    /// <summary>
    /// Object to hold information about each command
    /// </summary>
    class CommandRegistration
    {
        public string Command { get; private set; }
        public CommandHandler Handler { get; private set; }

        public CommandRegistration(string command, CommandHandler handler, string help)
        {
            Command = command;
            Handler = handler;
        }
    }

    /// <summary>
    /// How many log lines should be retained?
    /// Note that strings submitted to appendLogLine with embedded newlines will be counted as a single line.
    /// </summary>
    int ScrollbackSize = 20;

    Queue<string> _scrollback;
    List<string> _commandHistory = new List<string>();

    public string[] ScrollbackAry { get; private set; } //Copy of scrollback as an array for easier use by ConsoleView

    const string RepeatCmdName = "!!"; //Name of the repeat command, constant since it needs to skip these if they are in the command history

    

    public void AppendLogLine(string line)
    {
        if (_scrollback.Count >= ScrollbackSize)
        {
            _scrollback.Dequeue();
        }
        _scrollback.Enqueue(line);

        ScrollbackAry = _scrollback.ToArray();
        if (LogChanged != null)
        {
            LogChanged(ScrollbackAry);
        }
    }

    public void Log(string message)
    {
        AppendLogLine(" " + message);
        _commandHistory.Add(message);
    }
}