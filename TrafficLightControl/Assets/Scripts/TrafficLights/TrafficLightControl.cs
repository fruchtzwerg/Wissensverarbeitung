using UnityEngine;
using System.Collections;
using System;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.Text;

public class TrafficLightControl : MonoBehaviour, IProlog {

    public GameObject[] trafficLights;
    public string[] trafficLightNames;

    public GameObject PrologInterface;

    private const string GREEN = "G = ";
    private const string AMPEL = "ampel";

    public string CrossroadName;

    private Dictionary<string, TrafficLight> dictionary;
    private List<string> greenTrafficLights;

    private string phase;

    private Regex regex;
    // Use this for initialization
    void Start() {
        regex = new Regex(@"G\s=\s\[(gruen\(.*\),)*gruen\(.*\)\]\.");

        phase = "phase11";

        greenTrafficLights = new List<string>();

        if (trafficLights.Length == trafficLightNames.Length) {
            buildDictionary();
        }else {
            print("WARNING! Dircionary wird nicht richtig aufgebaut, da die Arrays von Namen und Gameobjekten unterschiedlich groß sind!");
        }            
    }

    // Update is called once per frame
    void Update() {

    }

    /// <summary>
    /// processing recived data
    /// </summary>
    /// <param name="recivedData"></param>
    public void ReciveDataFromProlog(string recivedData) {
        //recivedData is emtry or empty list
        if (string.IsNullOrEmpty(recivedData) || recivedData.Contains("G = []."))
            return;

        if (!regex.IsMatch(recivedData))
            print("Regex match nicht...");

        string recivedDataWithOutVar = recivedData.Replace(GREEN, "").Replace("[", "").Replace("]", "");

        string phasetmp = recivedDataWithOutVar.Substring(recivedDataWithOutVar.LastIndexOf(","));
        phasetmp = phasetmp.Replace(",", "").Replace(".","");
        phase = phasetmp.Trim();

        print(phase);

        string[] stringSeparators = new string[] { "gruen(" };
        var splits = recivedDataWithOutVar.Split(stringSeparators, StringSplitOptions.None);

        //clear old green elements
        greenTrafficLights.Clear();

        //remove unnecessary symbols and elements
        foreach (string s in splits) {
            var tmp = s.Replace(")", "").Replace(".", "").Replace(",", "");
            tmp = tmp.Trim();

            if (!string.IsNullOrEmpty(tmp))
                greenTrafficLights.Add(tmp);
            print(tmp);
        }
        
        //change states...
        foreach(var entry in dictionary) {
            //entry trafficlight in green trafficlight list?
            if (greenTrafficLights.Contains(entry.Key)) {

                print(entry.Key + " to green.");
                entry.Value.switchToGreen();
            }
                       
            else
                entry.Value.switchToRed();
        }        
    }

    public void NextState(string activator) {

        //string greenTrafficlightsList = this.buildGreenTrafficLightListString();
        //string activator = "'keineAktion'";

        //string query = "getnextPhase('" + CrossroadName + "', " + greenTrafficlightsList + ", " + activator + ", G).";
        string query = "getnextPhase('" + CrossroadName + "', '" + phase + "', " + activator + ", G).";
        PrologInterface.GetComponent<PrologWrapper>().QueryProlog(query, this);
    }

    /// <summary>
    /// map strings and trafficlightopjects to dictionary
    /// </summary>
    private void buildDictionary() {
        dictionary = new Dictionary<string, TrafficLight>();

        for(int i = 0; i < trafficLights.Length; i++) {
            dictionary.Add(trafficLightNames[i], trafficLights[i].GetComponent<TrafficLight>());
        }

    }

    [Obsolete]
    /// <summary>
    /// Build a string with all green trafficlights
    /// </summary>
    /// <returns></returns>
    private string buildGreenTrafficLightListString()
    {
        StringBuilder sb = new StringBuilder();

        foreach(var tmp in dictionary)
        {
            if (tmp.Value.State != TrafficLight.States.green)
                continue;

            sb.Append("gruen(" + tmp.Key + "), ");
        }

        string greenTrafficLights = sb.ToString();

        //no green trafficlight
        if (string.IsNullOrEmpty(greenTrafficLights))
            return "[]";


        //print();

        return "[" + greenTrafficLights.Remove(greenTrafficLights.Length - 2) + "]";
    }

}
