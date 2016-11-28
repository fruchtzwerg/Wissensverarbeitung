using UnityEngine;
using System.IO;
using System;
using System.Collections.Generic;

public class UnityLogger : MonoBehaviour {
    
    public ConsoleView ConsoleView;


    public void LogProlog(string message)
    {
        //ConsoleView.LogMessage(message);
        var msg = message;
        UnityThreadHelper.Dispatcher.Dispatch(() => ConsoleView.LogMessage(msg));
        print(message);
    }
}
