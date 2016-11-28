using UnityEngine;
using System;
using System.Collections.Generic;
using UnityThreading;
// KEEP THIS!!!
using UnityTimer = Timer;

public class UnityLogger
{
    
    private ConsoleView _consoleView;
    public static readonly string DELIMITER_SEND = "?- ";
    public static readonly string DELIMITER_RECEIVE = "    ";

    //private StreamWriter _swProlog;
    private List<Task> _tasks = new List<Task>();
    public System.Timers.Timer Timer;

    // Use this for initialization
    public UnityLogger(ConsoleView _consoleView)
    {
        this._consoleView =_consoleView;

        //if (Directory.Exists(@".\Assets\Prolog"))
        //    _swProlog = new StreamWriter(@".\Assets\Prolog\prolog.log", true);
        //else
        //    _swProlog = new StreamWriter(@".\Prolog\prolog.log", true);

        Timer = new System.Timers.Timer(1000) {AutoReset = true};
        Timer.Elapsed += (sender, args) => _tasks.RemoveAll(task => task.HasEnded);
        Timer.Start();
    }

    ~UnityLogger()
    {
        foreach (var task in _tasks)
            task.Abort();
        //_swProlog.Close();
    }

    public void Log(string message)
    {
        var msg = message;
        var task = UnityThreadHelper.Dispatcher.Dispatch(() => LogProlog(msg));
        _tasks.Add(task);
    }

    private void LogProlog(string message)
    {
        //ConsoleView.LogMessage(message);
        var msg = message;
        //UnityThreadHelper.Dispatcher.Dispatch(() => ConsoleView.LogMessage(msg));
        _consoleView.LogMessage(msg);
        //print(message);
        Debug.Log(message);

        //message = DateTime.Now + ": " + message;

        ////_swProlog.WriteLine(message);
        ////_swProlog.Flush();

        //_prologQueue.Enqueue(message);

        //if (_prologQueue.Count > MaxQueueCount)
        //{
        //    _prologQueue.Dequeue();
        //}

        //Debug.Log(message);
    }
}