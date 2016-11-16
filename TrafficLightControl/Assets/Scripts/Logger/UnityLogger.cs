using UnityEngine;
using System.Collections;
using System.IO;
using System;
using System.Collections.Generic;

public class UnityLogger : MonoBehaviour {

    private Queue<string> prologQueue;
    public int maxQueueCount = 10;

    StreamWriter swProlog;

    // Use this for initialization
    void Start () {
        prologQueue = new Queue<string>();

        swProlog = new StreamWriter(@".\Assets\Prolog\prolog.log", true);
    }
	
	// Update is called once per frame
	void Update () {
	}

    public void logProlog(string message){

        message = DateTime.Now + ": " + message;

        swProlog.WriteLine(message);
        swProlog.Flush();

        prologQueue.Enqueue(message);

        if (prologQueue.Count > maxQueueCount){
            prologQueue.Dequeue();
        }

        //Debug.Log(message);
    }

    public void logEvents(string eventMessage){

    }  
}
