using System;
using UnityEngine;
using System.Collections.Generic;
using System.Linq;
using MoreLinq;
using UnityEngine.EventSystems;
using UnityEngine.UI;
using UnityStandardAssets.CrossPlatformInput;

public class UI : MonoBehaviour
{

    public Vector3 CamPos1 = new Vector3(99.4f, 88.5f, 49.7f);
    public Vector3 CamPos2 = new Vector3(79.4f, 34.4f, 59.6f);
    public Vector3 CamPos3 = new Vector3(125.5f, 34.4f, 43.7f);
    public Vector3 CamPos4 = new Vector3(29.0f, 26.4f, 41.8f);

    public Button ButtonCamPos1;
    public Button ButtonCamPos2;
    public Button ButtonCamPos3;
    public Button ButtonCamPos4;

    public Button TabAButton;
    public Button TabBButton;
    public Button TabSpawnButton;

    public TrafficLightControl CrossroadControlA;
    public TrafficLightControl CrossroadControlB;

    public CamMoving Cam;    

    public Slider PaceSlider;
    public InputField PaceInput;
    
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

    public InputField CarsInput;

    private HorizontalLayoutGroup _rowPrefab;
    private Button _buttonPrefab;
    private Toggle _togglePrefab;

    public GameObject[] TimerMultiplier;

    // Use this for initialization
    void Awake()
    {
        ButtonCamPos1.onClick.AddListener(() => Cam.SetCamPosition(CamPos1));
        ButtonCamPos2.onClick.AddListener(() => Cam.SetCamPosition(CamPos2));
        ButtonCamPos3.onClick.AddListener(() => Cam.SetCamPosition(CamPos3));
        ButtonCamPos4.onClick.AddListener(() => Cam.SetCamPosition(CamPos4));
        SpawnButton.onClick.AddListener(SpawnVehicle);

        PaceSlider.onValueChanged.AddListener(delegate { SliderEvent(); });

        _rowPrefab = Resources.Load<HorizontalLayoutGroup>("UI/Row");
        _buttonPrefab = Resources.Load<Button>("UI/Button");
        _togglePrefab = Resources.Load<Toggle>("UI/Toggle");

        InitRows(CrossroadControlA.TrafficLights, _rowsA, RowsAParent);
        InitRows(CrossroadControlB.TrafficLights, _rowsB, RowsBParent);

        InitButtons(CrossroadControlA, ButtonsAParent);
        InitButtons(CrossroadControlB, ButtonsBParent);

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
            var toggle = Instantiate(_togglePrefab);
            toggle.name = wp.name;

            var text = toggle.GetComponentInChildren<Text>();
            text.text = wp.name;

            toggle.transform.SetParent(wp.IsOrigin
                                       ? OriginsParent.transform
                                       : DestinationsParent.transform);
            toggle.group = wp.IsOrigin ? OriginsParent : DestinationsParent;
        }

        var origin = OriginsParent.GetComponentInChildren<Toggle>();
        var destination = DestinationsParent.GetComponentInChildren<Toggle>();

        origin.isOn = true;
        destination.isOn = true;

        var OriginMarkerParent = GameObject.Find("SpawnLocations");
        var DestinationMarkerParent = GameObject.Find("DespawnLocations");

        UIMouseOver.SelectedOrigin = OriginMarkerParent.transform.FindChild(origin.name).transform;
        UIMouseOver.SelectedDestination = DestinationMarkerParent.transform.FindChild(destination.name).transform;

        OriginMarkerParent.SetActive(false);
        DestinationMarkerParent.SetActive(false);
    }

    private void InitRows(IEnumerable<TrafficLight> lights, ICollection<HorizontalLayoutGroup> rows, Component parent)
    {
        foreach (var l in lights)
        {
            var row = Instantiate(_rowPrefab);

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
            var button = Instantiate(_buttonPrefab);

            var text = button.GetComponentInChildren<Text>();
            text.text = @event.ToString();
            
            var e = @event; // necessary for lambda closure
            button.onClick.AddListener(() => TriggerEvent(e, control));

            button.transform.SetParent(parent.transform);
        }
    }
    #endregion
    

    // Update is called once per frame
    void Update()
    {
        SetTextOfInputField(CrossroadControlA.TrafficLights, _rowsA);
        SetTextOfInputField(CrossroadControlB.TrafficLights, _rowsB);

        // controller cam pos
        if(CrossPlatformInputManager.GetButton("Fire1"))
            Cam.SetCamPosition(CamPos2);
        if (CrossPlatformInputManager.GetButton("Fire2"))
            Cam.SetCamPosition(CamPos3);
        if (CrossPlatformInputManager.GetButton("Fire3"))
            Cam.SetCamPosition(CamPos4);
        if (CrossPlatformInputManager.GetButton("Jump"))
            Cam.SetCamPosition(CamPos1);

        if (!CarsInput.isFocused)
        {
            CarsInput.text = VehicleSpawner.Count.ToString();
        }
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
    private void AddOptionsToDropDown(Dropdown dropdown)
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


    private void TriggerEvent(EventTrigger.Events @event, TrafficLightControl control)
    {
        control.EventWasTriggered(@event);
    }


    void SliderEvent()
    {
        float value = PaceSlider.value;
        PaceInput.text = value.ToString();

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


    public void EndEditTextPace(string text)
    {
        PaceSlider.value = (float) Convert.ToDouble(text);
    }

    public void EndEditTextCount(string text)
    {
        Spawner.MaxVehicles = int.Parse(text);
    }

    public void HighlightTabA()
    {
        HighlightTab(true, false, false);
    }

    public void HighlightTabB()
    {
        HighlightTab(tabA: false, tabB: true, tabSpawn: false);
    }

    public void HighlightTabSpawn()
    {
        HighlightTab(tabA: false, tabB: false, tabSpawn: true);
    }

    private void HighlightTab(bool tabA, bool tabB, bool tabSpawn)
    {

        TabAButton.ChangeAlpha(tabA ? 0xC0 : 0x80);
        TabAButton.GetComponentInChildren<Text>().ChangeAlpha(tabA ? 0xFF : 0xC0);

        TabBButton.ChangeAlpha(tabB ? 0xC0 : 0x80);
        TabBButton.GetComponentInChildren<Text>().ChangeAlpha(tabB ? 0xFF : 0xC0);

        TabSpawnButton.ChangeAlpha(tabSpawn ? 0xC0 : 0x80);
        TabSpawnButton.GetComponentInChildren<Text>().ChangeAlpha(tabSpawn ? 0xFF : 0xC0);
    }
}
