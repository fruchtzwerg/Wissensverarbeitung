using UnityEngine;
using System.Collections;
using System.Timers;
using System;

public class PedestrianTrafficLight : TrafficLight, IIntervalMultiplierUpdate {

    public PedestrianButton[] PedestrianTrafficLightButtons;

    public long Interval = 2000;

    private AudioSource audioSource;
    private AudioClip clip1;
    private AudioClip clip2;

    protected override void InitTimerGreen() {
        timerGreen = new Timer
        {
            Interval = Interval,
            AutoReset = false
        };
        timerGreen.Elapsed += timerEventToGreen;

        initAudio();
    }

    private void initAudio() {
        gameObject.AddComponent<AudioSource>();

        try {           
            clip1 = Resources.Load("pedestrian-crossing_1") as AudioClip;
            clip2 = Resources.Load("pedestrian-crossing_2") as AudioClip;
        }
        catch(Exception e) {
            print("ERROR: " + e);
        }

        if (clip1 != null)
            clip1.LoadAudioData();
        else
            print("clip1 is null");
        if(clip2 != null)
            clip2.LoadAudioData();
        else
            print("clip2 is null");

        audioSource = gameObject.GetComponent<AudioSource>();
        audioSource.playOnAwake = true;
        audioSource.loop = true;
        audioSource.maxDistance = 10;
        audioSource.minDistance = 0.3f;
        audioSource.clip = clip1;
        audioSource.spatialBlend = 1f;
        audioSource.Play();

        print("Sounds: " + audioSource.clip);
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

                    changeAudioClip(true);
                    break;
                case States.Orange:
                    rendRed.material.SetColor("_EmissionColor", Color.black);
                    rendGreen.material.SetColor("_EmissionColor", Color.black);
                    break;
                case States.Green:
                    rendRed.material.SetColor("_EmissionColor", Color.black);
                    rendGreen.material.SetColor("_EmissionColor", Color.white);

                    switchOffPedestrianTrafficLightLights();

                    changeAudioClip(false);
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

    private void changeAudioClip(bool clip) {
        if (clip)
            audioSource.clip = clip2;
        else
            audioSource.clip = clip1;
        audioSource.Play();
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
