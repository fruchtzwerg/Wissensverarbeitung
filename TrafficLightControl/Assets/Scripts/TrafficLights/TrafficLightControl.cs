using UnityEngine;
using System.Collections;
using System;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.Text;
using System.Timers;

public class TrafficLightControl : MonoBehaviour, IProlog {

    public GameObject[] trafficLights;
    public string[] trafficLightNames;

    public GameObject PrologInterface;

    private const string GREEN = "G = ";
    private const string AMPEL = "ampel";

    public string CrossroadName;

    private Dictionary<string, TrafficLight> dictionary;
    private List<string> greenTrafficLights;

    private PrologWrapper wrapper;
    private Regex regex;
    private string phase;

    private Timer timerNormalState;
    public long backToNormalStateInterval = 8000;

    // Use this for initialization
    void Start() {
        regex = new Regex(@"G\s=\s\[(gruen\(.*\),)*gruen\(.*\)\]\.");
        wrapper = PrologInterface.GetComponent<PrologWrapper>();

        phase = "phase11";
        greenTrafficLights = new List<string>();        

        if (trafficLights.Length == trafficLightNames.Length) {
            BuildDictionary();
        }else {
            print("WARNING! Dircionary wird nicht richtig aufgebaut, da die Arrays von Namen und Gameobjekten unterschiedlich groß sind!");
        }

        timerNormalState = new Timer();
        timerNormalState.Interval = backToNormalStateInterval;
        timerNormalState.AutoReset = false;
        timerNormalState.Elapsed += TimerEvent;          
    }

    // Update is called once per frame
    void Update() {

    }

    // Kill swi-prolog.exe when unity quits.
    void OnApplicationQuit() {
        timerNormalState.Stop();
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
        recivedDataWithOutVar = recivedDataWithOutVar.Replace(phasetmp, "");
        phasetmp = phasetmp.Replace(",", "").Replace(".","");
        phase = phasetmp.Trim();
                
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
            //print(tmp);
        }
        
        //change states...
        foreach(var entry in dictionary) {
            //entry trafficlight in green trafficlight list?
            if (greenTrafficLights.Contains(entry.Key)) {

                //print(entry.Key + " to green.");
                entry.Value.switchToGreen();
            }
                       
            else
                entry.Value.switchToRed();
        }        
    }

    /// <summary>
    /// Call this function to got to the next state
    /// </summary>
    /// <param name="activator"></param>
    public void NextState(string activator) {

        timerNormalState.Stop();
        
        string query = "getnextPhase('" + CrossroadName + "', '" + phase + "', " + activator + ", G).";
        wrapper.QueryProlog(query, this);

        //Start timer to normal state
        timerNormalState.Start();
    }

    /// <summary>
    /// map strings and trafficlightopjects to dictionary
    /// </summary>
    private void BuildDictionary() {
        dictionary = new Dictionary<string, TrafficLight>();

        for(int i = 0; i < trafficLights.Length; i++) {
            dictionary.Add(trafficLightNames[i], trafficLights[i].GetComponent<TrafficLight>());
        }

    }

    #region string with green trafficlights
    [Obsolete]
    /// <summary>
    /// Build a string with all green trafficlights
    /// </summary>
    /// <returns></returns>
    private string BuildGreenTrafficLightListString()
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
    #endregion

    private void TimerEvent(object sender, System.EventArgs e) {

        string query = "getnextPhase('" + CrossroadName + "', '" + phase + "', " + "keineAktion" + ", G).";
        wrapper.QueryProlog(query, this);

        print("Next State: normal mode");
    }
}
