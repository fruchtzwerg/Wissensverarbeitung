using UnityEngine;
using System.Collections;
using System.Timers;
using System;

public class PedestrianTrafficLight : TrafficLight, IIntervalMultiplierUpdate {

    public PedestrianButton[] PedestrianTrafficLightButtons;

    public long Interval = 2000;

    protected override void InitTimerGreen() {
        timerGreen = new Timer
        {
            Interval = Interval,
            AutoReset = false
        };
        timerGreen.Elapsed += timerEventToGreen;
    }

    /// <summary>
    /// switch state from red to green
    /// </summary>
    public override void switchToGreen() {
        //only if red or in some sec red
        if (State != States.Green) {
            timerGreen.Start();
        }
    }

    /// <summary>
    /// switch strate from green to red
    /// </summary>
    public override void switchToRed() {
        //only if red or in some sec red
        if (State != States.Red) {
            state = States.Red;
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
                case States.Red:
                    rendRed.material.SetColor("_EmissionColor", Color.white);
                    rendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
                case States.Orange:
                    rendRed.material.SetColor("_EmissionColor", Color.black);
                    rendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
                case States.Green:
                    rendRed.material.SetColor("_EmissionColor", Color.black);
                    rendGreen.material.SetColor("_EmissionColor", Color.white);

                    switchOffPedestrianTrafficLightLights();
                    break;
                case States.RedAndOrange:
                    rendRed.material.SetColor("_EmissionColor", Color.white);
                    rendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
                case States.On:
                    rendRed.material.SetColor("_EmissionColor", Color.white);
                    rendGreen.material.SetColor("_EmissionColor", Color.white);
                    break;
                default:
                    rendRed.material.SetColor("_EmissionColor", Color.black);
                    rendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
            }
        }
    }

    /// <summary>
    /// switch off all PedestrianTrafficLight Lights
    /// </summary>
    private void switchOffPedestrianTrafficLightLights() {
        foreach(var tmp in PedestrianTrafficLightButtons) {
            tmp.switchOffEmission();
        }
    }

    public new void updateMultiplier(float value)
    {
        timerGreen.Interval = (long)(Interval * value);
    }
}
