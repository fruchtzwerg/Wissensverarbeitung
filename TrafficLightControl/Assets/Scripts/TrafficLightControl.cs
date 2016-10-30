using UnityEngine;
using System.Collections;
using System;

public class TrafficLightControl : MonoBehaviour, IProlog {

    public GameObject[] trafficLights;

    public GameObject PrologInterface;

    private const string RED = "R = ";
    private const string GREEN = "G = ";

    // Use this for initialization
    void Start () {
	
	}
	
	// Update is called once per frame
	void Update () {
	
	}

    public void ReciveDataFromProlog(string recivedData) {


        int startPosRed = recivedData.IndexOf(RED);
        int startPosGreen = recivedData.IndexOf(GREEN);

        string red = recivedData.Substring(startPosRed, startPosGreen);
        string green = recivedData.Substring(startPosGreen);

        print("RED: " + red);
        print("GREEN: " + green);
    }

    public void NextState() {

        PrologInterface.GetComponent<PrologWrapper>().QueryProlog("gib_Weltzustand(weltzustand1, R, G).", this);
    }
}
