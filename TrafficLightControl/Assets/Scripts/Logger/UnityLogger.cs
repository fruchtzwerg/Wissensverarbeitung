using UnityEngine;
using System.Collections;
using System.IO;
using System;

public class UnityLogger : MonoBehaviour {

    StreamWriter swProlog;

	// Use this for initialization
	void Start () {
        //swProlog = new StreamWriter(@".\Assets\Prolog\prolog.log", true);
    }
	
	// Update is called once per frame
	void Update () {
	
	}

    public void logProlog(string message){
        using (StreamWriter sw2 = new StreamWriter(@".\Assets\Prolog\prolog.log", true))
        {
            sw2.WriteLine(DateTime.Now + ": " + message);
            sw2.Flush();
        }


        Debug.Log(message);
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
