using System;
using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Security.Policy;
using MoreLinq;
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

    public ToggleGroup OriginsParent;
    public ToggleGroup DestinationsParent;

    public Button SpawnButton;
    public VehicleSpawner Spawner;

    public InputField Cars;

    private HorizontalLayoutGroup rowPrefab;
    private Button buttonPrefab;
    private Toggle togglePrefab;

    public GameObject[] TimerMultiplier;

    // Use this for initialization
    void Awake()
    {
        buttonCamPos1.onClick.AddListener(() => Cam.SetCamPosition(camPos1));
        buttonCamPos2.onClick.AddListener(() => Cam.SetCamPosition(camPos2));
        buttonCamPos3.onClick.AddListener(() => Cam.SetCamPosition(camPos3));
        buttonCamPos4.onClick.AddListener(() => Cam.SetCamPosition(camPos4));
        SpawnButton.onClick.AddListener(() => SpawnVehicle());

        paceSlider.onValueChanged.AddListener(delegate { SliderEvent(); });

        rowPrefab = Resources.Load<HorizontalLayoutGroup>("UI/Row");
        buttonPrefab = Resources.Load<Button>("UI/Button");
        togglePrefab = Resources.Load<Toggle>("UI/Toggle");

        InitRows(CrossroadControl_A.TrafficLights, _rowsA, RowsAParent);
        InitRows(CrossroadControl_B.TrafficLights, _rowsB, RowsBParent);

        InitButtons(CrossroadControl_A, ButtonsAParent);
        InitButtons(CrossroadControl_B, ButtonsBParent);

        InitToggles();
    }

    void Start()
    {
        SliderEvent();
    }


    #region init
    private void InitToggles()
    {
        var lanesParents = GameObject.FindGameObjectsWithTag("Lanes");
        var waypoints = (from lane in lanesParents
            from waypoint in lane.GetComponentsInChildren<SplineWaypoint>()
            where waypoint.IsOrigin || waypoint.IsDestination
            select waypoint).ToArray().DistinctBy(wp => wp.name);

        foreach (var wp in waypoints)
        {
            var toggle = Instantiate(togglePrefab);
            toggle.name = wp.name;

            var text = toggle.GetComponentInChildren<Text>();
            text.text = wp.name;

            toggle.transform.SetParent(wp.IsOrigin
                                       ? OriginsParent.transform
                                       : DestinationsParent.transform);
            toggle.group = wp.IsOrigin ? OriginsParent : DestinationsParent;
        }

        OriginsParent.GetComponentInChildren<Toggle>().isOn = true;
        DestinationsParent.GetComponentInChildren<Toggle>().isOn = true;
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
            button.onClick.AddListener(() => TriggerEvent(e.ToString(), control));

            button.transform.SetParent(parent.transform);
        }
    }
    #endregion

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

        Cars.text = VehicleSpawner.Count.ToString();
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
            var background = name.GetComponent<Image>();
            
            if (lights[i].GetType() == typeof(BoomGateController))
            {
                var entry = (BoomGate) lights[i];
                name.text = entry.State.ToString();

                switch (entry.State)
                {
                    case BoomGate.States.Open:
                        background.color = Color.green;
                        break;
                    case BoomGate.States.Closed:
                        background.color = Color.red;
                        break;
                    default:
                        background.color = Color.yellow;
                        break;
                }
            }
            else
            {
                var state = lights[i].State;
                name.text = state.ToString();

                switch (state)
                {
                    case TrafficLight.States.Red:
                        background.color = Color.red;
                        break;
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


    //##########################################  Events  ##########################################################

    private void SpawnVehicle()
    {
        var origin = (from o in OriginsParent.GetComponentsInChildren<Toggle>()
                     where o.isOn
                     select o).FirstOrDefault();

        var destination = (from d in DestinationsParent.GetComponentsInChildren<Toggle>()
                          where d.isOn
                          select d).FirstOrDefault();

        Spawner.Spawn(origin.name, destination.name);
    }


    private void TriggerEvent(string @event, TrafficLightControl control)
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
