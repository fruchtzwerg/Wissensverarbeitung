using UnityEngine;
using System.IO;
using System;
using System.Collections.Generic;

public class UnityLogger : MonoBehaviour {

    private Queue<string> _prologQueue;
    public int MaxQueueCount = 10;
    public ConsoleView ConsoleView;

    private StreamWriter _swProlog;

    // Use this for initialization
    void Start () {
        _prologQueue = new Queue<string>();

        _swProlog = new StreamWriter(@".\Assets\Prolog\prolog.log", true);
    }

    void OnApplicationQuit()
    {
        _swProlog.Close();
    }

    public void LogProlog(string message){

        ConsoleView.Log(message);

        message = DateTime.Now + ": " + message;

        _swProlog.WriteLine(message);
        _swProlog.Flush();

        _prologQueue.Enqueue(message);

        if (_prologQueue.Count > MaxQueueCount){
            _prologQueue.Dequeue();
        }

        //Debug.Log(message);
    }

    public void LogEvents(string eventMessage){

    }
}
