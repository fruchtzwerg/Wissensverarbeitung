using System;
using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using UnityEngine.UI;
using UnityStandardAssets.CrossPlatformInput;

public class UI : MonoBehaviour
{

    public Vector3 camPos1 = new Vector3(99.4f, 88.5f, 49.7f);
    public Vector3 camPos2 = new Vector3(79.4f, 34.4f, 59.6f);
    public Vector3 camPos3 = new Vector3(125.5f, 34.4f, 43.7f);
    public Vector3 camPos4 = new Vector3(29.0f, 26.4f, 41.8f);

    public Button buttonCamPos1;
    public Button buttonCamPos2;
    public Button buttonCamPos3;
    public Button buttonCamPos4;

    public TrafficLightControl CrossroadControl_A;
    public TrafficLightControl CrossroadControl_B;

    public CamMoving Cam;    

    public Slider paceSlider;
    public InputField paceInput;
    
    public VerticalLayoutGroup RowsAParent;
    public GridLayoutGroup ButtonsAParent;
    private List<HorizontalLayoutGroup> _rowsA = new List<HorizontalLayoutGroup>();

    public VerticalLayoutGroup RowsBParent;
    public GridLayoutGroup ButtonsBParent;
    private List<HorizontalLayoutGroup> _rowsB = new List<HorizontalLayoutGroup>();

    private HorizontalLayoutGroup rowPrefab;
    private Button buttonPrefab;

    public GameObject[] TimerMultiplier;

    // Use this for initialization
    void Start()
    {
        buttonCamPos1.onClick.AddListener(() => Cam.SetCamPosition(camPos1));
        buttonCamPos2.onClick.AddListener(() => Cam.SetCamPosition(camPos2));
        buttonCamPos3.onClick.AddListener(() => Cam.SetCamPosition(camPos3));
        buttonCamPos4.onClick.AddListener(() => Cam.SetCamPosition(camPos4));

        paceSlider.onValueChanged.AddListener(delegate { SliderEvent(); });

        rowPrefab = Resources.Load<HorizontalLayoutGroup>("Row");
        buttonPrefab = Resources.Load<Button>("Button");

        InitRows(CrossroadControl_A.TrafficLights, _rowsA, RowsAParent);
        InitRows(CrossroadControl_B.TrafficLights, _rowsB, RowsBParent);

        InitButtons(CrossroadControl_A, ButtonsAParent);
        InitButtons(CrossroadControl_B, ButtonsBParent);
    }

    private void InitRows(IEnumerable<TrafficLight> lights, ICollection<HorizontalLayoutGroup> rows, Component parent)
    {
        foreach (var l in lights)
        {
            var row = Instantiate(rowPrefab);
            var name = row.GetComponentInChildren<Text>();
            var state = row.GetComponentInChildren<InputField>();

            name.text = l.Name.ToString();
            state.text = l.State.ToString();
            row.transform.SetParent(parent.transform, false);

            rows.Add(row);
        }
    }

    private void InitButtons(TrafficLightControl control, Component parent)
    {
        var events = control.GetComponent<EventTrigger>().events;
        foreach (var @event in events)
        {
            var button = Instantiate(buttonPrefab);
            var text = button.GetComponentInChildren<Text>();

            text.text = @event.ToString();
            
            var e = @event; // necessary for lambda closure
            button.onClick.AddListener(() => OnButtonClicked(e.ToString(), control));

            button.transform.SetParent(parent.transform);
        }
    }

    // Update is called once per frame
    void Update()
    {
        SetTextOfInputField(CrossroadControl_A.TrafficLights, _rowsA);
        SetTextOfInputField(CrossroadControl_B.TrafficLights, _rowsB);

        // controller cam pos
        if(CrossPlatformInputManager.GetButton("Fire1"))
            Cam.SetCamPosition(camPos2);
        if (CrossPlatformInputManager.GetButton("Fire2"))
            Cam.SetCamPosition(camPos3);
        if (CrossPlatformInputManager.GetButton("Fire3"))
            Cam.SetCamPosition(camPos4);
        if (CrossPlatformInputManager.GetButton("Jump"))
            Cam.SetCamPosition(camPos1);
    }

    //####################################################################################################
    /// <summary>
    ///  set the text of the inputfield array
    /// </summary>
    private void SetTextOfInputField(IList<TrafficLight> lights, IList<HorizontalLayoutGroup> rows)
    {
        for (var i = 0; i < lights.Count; i++)
        {
            var name = rows[i].GetComponentInChildren<InputField>();
            var state = lights[i].State;
            var background = name.GetComponent<Image>();
            name.text = state.ToString();

            switch (state)
            {
                case TrafficLight.States.Closed:
                case TrafficLight.States.Red:
                    background.color = Color.red;
                    break;
                case TrafficLight.States.Open:
                case TrafficLight.States.Green:
                    background.color = Color.green;
                    break;
                case TrafficLight.States.Off:
                    background.color = Color.white;
                    break;
                default:
                    background.color = Color.yellow;
                    break;
            }
        }
    }

    /// <summary>
    /// add options to dropdown
    /// </summary>
    /// <param name="dropdown"></param>
    private void addOptionsToDropDown(Dropdown dropdown)
    {
        dropdown.options.Clear();

        //add Options to dropdown
        foreach (string state in System.Enum.GetNames(typeof(TrafficLight.States)))
        {
            dropdown.options.Add(new Dropdown.OptionData() {text = state});
        }
    }


    //########################################## Events  ##########################################################

    private void OnButtonClicked(string @event, TrafficLightControl control)
    {
        control.EventWasTriggered(@event);
    }


    void SliderEvent()
    {
        float value = paceSlider.value;
        paceInput.text = value.ToString();

        float multiplier = 1f/value;
        // for each registered object
        foreach (var tmp1 in TimerMultiplier)
        {
            // get interfaces of its children
            var childrenMultiplier = tmp1.GetComponentsInChildren<IIntervalMultiplierUpdate>();

            // for each interface of its children
            foreach (var tmp2 in childrenMultiplier)
            {
                // update childrens interfaces
                tmp2.updateMultiplier(multiplier);
            }

            // update registered object (parent) interface
            var parent = tmp1.GetComponent<IIntervalMultiplierUpdate>();
            if (parent != null)
                parent.updateMultiplier(multiplier);
        }
    }


    public void EndEditText(string text)
    {
        paceSlider.value = (float) Convert.ToDouble(text);
    }
}
