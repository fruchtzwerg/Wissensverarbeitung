using UnityEngine;
using System.Collections;
using System.IO;
using System;
using System.Collections.Generic;

public class UnityLogger : MonoBehaviour {

    private Queue<string> prologQueue;
    public int maxQueueCount = 10;

    // Use this for initialization
    void Start () {
        prologQueue = new Queue<string>();
    }
	
	// Update is called once per frame
	void Update () {
	}

    public void logProlog(string message){

        message = DateTime.Now + ": " + message;

        using (StreamWriter sw2 = new StreamWriter(@".\Assets\Prolog\prolog.log", true))
        {
            sw2.WriteLine(message);
            sw2.Flush();
        }

        prologQueue.Enqueue(message);

        if (prologQueue.Count > maxQueueCount){
            prologQueue.Dequeue();
        }

        //Debug.Log(message);
    }

    public void logEvents(string eventMessage){

    }


    /// <summary>
    /// Write to logfile
    /// </summary>
    /// <param name="message"></param>
    private void WriteLogFile(string message)
    {
        using (StreamWriter sw2 = new StreamWriter(@".\Assets\Prolog\prolog.log", true))
        {
            sw2.WriteLine(DateTime.Now + ": " + message);
            sw2.Flush();
        }

        Debug.Log(message);
    }    
}
