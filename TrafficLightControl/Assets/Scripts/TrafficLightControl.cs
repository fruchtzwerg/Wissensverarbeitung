using UnityEngine;
using System.Collections;
using System;
using System.Text.RegularExpressions;
using System.Collections.Generic;

public class TrafficLightControl : MonoBehaviour, IProlog {

    public GameObject[] trafficLights;

    public GameObject PrologInterface;

    private const string GREEN = "G = ";
    private const string AMPEL = "ampel";

    private Regex regex;
    // Use this for initialization
    void Start() {
        regex = new Regex(@"G\s=\s\[(gruen\(.*\),)*gruen\(.*\)\]\.");
    }

    // Update is called once per frame
    void Update() {

    }

    public void ReciveDataFromProlog(string recivedData) {
        if (String.IsNullOrEmpty(recivedData))
            return;


        if (!regex.IsMatch(recivedData))
            print("Regex match nicht...");

        string recivedDataWithOutVar = recivedData.Replace("R = ", "").Replace("[", "").Replace("]", "");

        string[] stringSeparators = new string[] { "gruen(" };
        var splits = recivedDataWithOutVar.Split(stringSeparators, StringSplitOptions.None);

        List<String> greenTrafficLights = new List<String>();

        foreach (string s in splits) {
            var tmp = s.Replace(")", "").Replace(".", "").Replace(",", "");
            tmp = tmp.Trim();

            if (!String.IsNullOrEmpty(tmp))
                greenTrafficLights.Add(tmp);
        }

        //TODO Strings auswerten nach Ampel und dann grün oder rot schalten
        /*foreach (string s in greenTrafficLights) {
            if (s.Equals(AMPEL + 1)) {
                var trafficLight = trafficLights[0].GetComponent<TrafficLight>();
                trafficLight.switchToGreen();
            }
        } */ 
        

        print("GREEN onse: ");

        foreach (String s in greenTrafficLights)
            print(s);

        
    }

    public void NextState() {

        PrologInterface.GetComponent<PrologWrapper>().QueryProlog("gib_Weltzustand(weltzustand1, R).", this);
    }
}
