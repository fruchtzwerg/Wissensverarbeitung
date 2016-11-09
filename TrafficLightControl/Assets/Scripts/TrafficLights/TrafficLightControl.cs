using UnityEngine;
using System.Collections;
using System;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using System.Text;
using System.Timers;

public class TrafficLightControl : MonoBehaviour, IProlog, IIntervalMultiplierUpdate {

    public GameObject[] trafficLights;
    public string[] trafficLightNames;

    public GameObject PrologInterface;

    private const string GREEN = "G = ";
    private const string AMPEL = "ampel";
    private const string NEXT_PHASE = "getnextPhase(";
    private const string NEUES_EREIGNIS = "neuesEreignis(";

    public string CrossroadName;

    private Dictionary<string, TrafficLight> dictionary;
    private List<string> greenTrafficLights;

    private PrologWrapper wrapper;
    private string phase;

    private Timer phaseTimer;
    private float multiplier = 1.0f;

    // Use this for initialization
    void Start() {
        wrapper = PrologInterface.GetComponent<PrologWrapper>();

        phase = "phase11";
        greenTrafficLights = new List<string>();        

        if (trafficLights.Length == trafficLightNames.Length) {
            BuildDictionary();
        }else {
            print("WARNING! Dircionary wird nicht richtig aufgebaut, da die Arrays von Namen und Gameobjekten unterschiedlich groß sind!");
        }

        phaseTimer = new Timer();
        phaseTimer.Interval = 2000;
        phaseTimer.AutoReset = false;
        phaseTimer.Elapsed += TimerEvent;
        phaseTimer.Start();        
    }

    // Kill swi-prolog.exe when unity quits.
    void OnApplicationQuit() {
        if(phaseTimer != null)
            phaseTimer.Stop();
    }

    /// <summary>
    /// processing recived data
    /// </summary>
    /// <param name="recivedData"></param>
    public void ReciveDataFromProlog(string recivedData) {
        //recivedData is emtry or empty list
        if (string.IsNullOrEmpty(recivedData) || recivedData.Contains("G = [].") || recivedData.Equals("true") || recivedData.Equals("false") || recivedData.Equals("true.") || recivedData.Equals("false."))
            return;


        //\[\[.*\],.*,\d{1,3}\]
        //print("R:" + recivedData);
        try {
            string recivedDataWithOutVar = recivedData.Replace(GREEN, "");

            //print("Rwov:" + recivedData);

            int arrayStart = recivedDataWithOutVar.LastIndexOf("[");
            int arrayEnd = recivedDataWithOutVar.IndexOf("]");
            int arrayLength = arrayEnd - arrayStart;

            //print("S:"+ arrayStart + ", E:"+ arrayEnd + ", L:"+ arrayLength);

            string greenTrafficLightsArray = recivedDataWithOutVar.Substring(arrayStart, arrayLength);
            greenTrafficLightsArray = greenTrafficLightsArray.Replace("[", "").Replace("]", "");

            string[] stringSeparators = new string[] { "," };
            var splits = greenTrafficLightsArray.Split(stringSeparators, StringSplitOptions.None);

            //clear old green elements
            greenTrafficLights.Clear();

            //remove unnecessary symbols and elements and add elemt to list
            foreach (string s in splits) {
                var tmp = s.Replace(")", "").Replace(".", "").Replace(",", "");
                tmp = tmp.Trim();

                if (!string.IsNullOrEmpty(tmp))
                    greenTrafficLights.Add(tmp);
                //print(tmp);
            }

            //change states...
            foreach (var entry in dictionary) {
                //entry trafficlight in green trafficlight list?
                if (greenTrafficLights.Contains(entry.Key)) {

                    //print(entry.Key + " to green.");
                    entry.Value.switchToGreen();
                }

                else
                    entry.Value.switchToRed();
            }

            //phase time from response
            int phaseTimeStartIndex = recivedDataWithOutVar.LastIndexOf(",");

            string nextPhaseTimeString = recivedDataWithOutVar.Substring(phaseTimeStartIndex).Replace("].", "").Replace(",", "").Trim();


            int nextPhaseTime = Convert.ToInt32(nextPhaseTimeString);

            phaseTimer.Interval = (long)(nextPhaseTime * 1000 * multiplier);

            phaseTimer.Start();
        }
        catch (Exception ex) {
            print("#######################################################################");
            print(ex);
            print("#######################################################################");
        }
    }

    /// <summary>
    /// Call this function to got to the next state
    /// </summary>
    /// <param name="activator"></param>
    private void NextState() {
                
        string query = NEXT_PHASE + CrossroadName + ", G).";
        wrapper.QueryProlog(query, this);       
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
    

    private void TimerEvent(object sender, System.EventArgs e) {
        NextState();
    }

    /// <summary>
    /// Call prolog, that a new event was triggered at this crossroad
    /// </summary>
    /// <param name="trigger"></param>
    public void EventWasTriggered(string trigger) {
        string query = NEUES_EREIGNIS + CrossroadName + ", " + trigger + ").";
        wrapper.QueryProlog(query);

        if (!phaseTimer.Enabled)
            print(">>>>>>>>>>>>>>>>>> TIMER IS NOT RUNNING <<<<<<<<<<<<<<<<<<<<<<<<<");
    }

    public void updateMultiplier(float value)
    {
        multiplier = value;
    }
}
