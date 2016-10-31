using UnityEngine;
using System.Collections;
using System.Timers;
using System;

public class TrafficLight : MonoBehaviour {
    


    public GameObject RedLight;
    public GameObject OrangeLight;
    public GameObject GreenLight;


    public Shader shader;
    
    private Renderer rendRed;
    private Renderer rendOrange;
    private Renderer rendGreen;

    private Color black = new Color(0, 0, 0);
    private Color red = new Color(1f, 0, 0);
    private Color orange = new Color(1f, 1f, 0);
    private Color green = new Color(0, 1f, 0);

    private Timer timerGreen;
    private Timer timerRed;

    public States state = States.off;
    private States oldState = States.off;
    public enum States {
        red,
        orange,
        green,
        redAndOrange,
        greenAndOrange,
        off,
        on
    }

    // Use this for initialization
    void Start () {

        //init Rendere with different materials
        rendRed = new Renderer();         

        rendRed = RedLight.GetComponent<Renderer>();
        rendRed.material = new Material(shader);
        rendRed.material.EnableKeyword("_EMISSION");
        rendRed.material.color = red;

        rendOrange = OrangeLight.GetComponent<Renderer>();
        rendOrange.material = new Material(shader);
        rendOrange.material.EnableKeyword("_EMISSION");
        rendOrange.material.color = orange;

        rendGreen = GreenLight.GetComponent<Renderer>();
        rendGreen.material = new Material(shader);
        rendGreen.material.EnableKeyword("_EMISSION");
        rendGreen.material.color = green;

        timerGreen = new Timer();
        timerGreen.Interval = 3000;
        timerGreen.AutoReset = false;
        timerGreen.Elapsed += timerEventToGreen;       

        timerRed = new Timer();
        timerRed.Interval = 2000;
        timerRed.AutoReset = false;
        timerRed.Elapsed += timerEventToRed;

        state = States.red;
        switchState();
    }

	// Update is called once per frame
	void Update () {
        switchState();
	}

    /// <summary>
    /// switch state from red to green
    /// </summary>
    public void switchToGreen() {
        //only if red or in some sec red
        if (state != States.redAndOrange || state != States.green) {
            state = States.redAndOrange;
            switchState();
            timerGreen.Start();
        }
    }

    /// <summary>
    /// switch strate from green to red
    /// </summary>
    public void switchToRed() {
        //only if red or in some sec red
        if (state != States.orange || state != States.red) {
            state = States.orange;
            switchState();
            timerRed.Start();
        }
    }

    private void timerEventToGreen(object source, EventArgs e) {
        //only state -> update called switchState -> timer has one thread and can't call to main thread
        state = States.green;
        timerGreen.Stop();
    }    

    private void timerEventToRed(object source, EventArgs e) {
        //only state -> update called switchState -> timer has one thread and can't call to main thread
        state = States.red;
        timerRed.Stop();
    }


    /// <summary>
    /// switch state and emission
    /// </summary>
    void switchState() {

        if (oldState != state) {
            oldState = state;
            //switch emissioncolor of gameobjects
            switch (state) {
                case States.red:
                    rendRed.material.SetColor("_EmissionColor", red);
                    rendOrange.material.SetColor("_EmissionColor", black);
                    rendGreen.material.SetColor("_EmissionColor", black);
                    break;
                case States.orange:
                    rendRed.material.SetColor("_EmissionColor", black);
                    rendOrange.material.SetColor("_EmissionColor", orange);
                    rendGreen.material.SetColor("_EmissionColor", black);
                    break;
                case States.green:
                    rendRed.material.SetColor("_EmissionColor", black);
                    rendOrange.material.SetColor("_EmissionColor", black);
                    rendGreen.material.SetColor("_EmissionColor", green);
                    break;
                case States.greenAndOrange:
                    rendRed.material.SetColor("_EmissionColor", black);
                    rendOrange.material.SetColor("_EmissionColor", orange);
                    rendGreen.material.SetColor("_EmissionColor", green);
                    break;
                case States.redAndOrange:
                    rendRed.material.SetColor("_EmissionColor", red);
                    rendOrange.material.SetColor("_EmissionColor", orange);
                    rendGreen.material.SetColor("_EmissionColor", black);
                    break;
                case States.on:
                    rendRed.material.SetColor("_EmissionColor", red);
                    rendOrange.material.SetColor("_EmissionColor", orange);
                    rendGreen.material.SetColor("_EmissionColor", green);
                    break;
                default:
                    rendRed.material.SetColor("_EmissionColor", black);
                    rendOrange.material.SetColor("_EmissionColor", black);
                    rendGreen.material.SetColor("_EmissionColor", black);
                    break;
            }

        }        
    }
}
