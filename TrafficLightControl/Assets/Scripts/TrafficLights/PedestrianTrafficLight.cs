using UnityEngine;
using System.Collections;
using System.Timers;

public class PedestrianTrafficLight : TrafficLight {

    public GameObject[] PedestrianTrafficLightButtons;


    protected override void InitTimerGreen() {
        timerGreen = new Timer();
        timerGreen.Interval = 2000;
        timerGreen.AutoReset = false;
        timerGreen.Elapsed += timerEventToGreen;
    }

    /// <summary>
    /// switch state from red to green
    /// </summary>
    public override void switchToGreen() {
        //only if red or in some sec red
        if (State != States.green) {
            timerGreen.Start();
        }
    }

    /// <summary>
    /// switch strate from green to red
    /// </summary>
    public override void switchToRed() {
        //only if red or in some sec red
        if (State != States.red) {
            state = States.red;
        }
    }

    /// <summary>
    /// switch state and emission
    /// </summary>
    protected override void switchState() {

        if (oldState != State) {
            oldState = State;
            //switch emissioncolor of gameobjects
            switch (State) {
                case States.red:
                    rendRed.material.SetColor("_EmissionColor", red);
                    rendGreen.material.SetColor("_EmissionColor", black);
                    break;
                case States.orange:
                    rendRed.material.SetColor("_EmissionColor", black);
                    rendGreen.material.SetColor("_EmissionColor", black);
                    break;
                case States.green:
                    rendRed.material.SetColor("_EmissionColor", black);
                    rendGreen.material.SetColor("_EmissionColor", green);

                    switchOffPedestrianTrafficLightLights();
                    break;
                case States.greenAndOrange:
                    rendRed.material.SetColor("_EmissionColor", black);
                    rendGreen.material.SetColor("_EmissionColor", green);
                    break;
                case States.redAndOrange:
                    rendRed.material.SetColor("_EmissionColor", red);
                    rendGreen.material.SetColor("_EmissionColor", black);
                    break;
                case States.on:
                    rendRed.material.SetColor("_EmissionColor", red);
                    rendGreen.material.SetColor("_EmissionColor", green);
                    break;
                default:
                    rendRed.material.SetColor("_EmissionColor", black);
                    rendGreen.material.SetColor("_EmissionColor", black);
                    break;
            }
        }
    }

    /// <summary>
    /// switch off all PedestrianTrafficLight Lights
    /// </summary>
    private void switchOffPedestrianTrafficLightLights() {
        foreach(var tmp in PedestrianTrafficLightButtons) {
            tmp.GetComponent<PedestrianButton>().switchOffEmission();
        }
    }
}
