using System;
using System.Collections.Generic;
using UnityEngine;
using Random = System.Random;

public class VehicleSpawner : MonoBehaviour, IIntervalMultiplierUpdate
{
    // spwan chances in percentage per frame
    // 1f = 1% chance each frame
    // @60 fps = 60% chance each second
    // = 1 car each 100 seconds on average
    public float CarSpawnChancePercentage = 5f;
    public float SuvSpawnChancePercentage = 1.5f;
    public float BusSpawnChancePercentage = .1f;
    public float TruckSpawnChancePercentage = .5f;

    public int MaxVehicles = 100;
    public static int Count;
    public Transform Lanes;

    private float _carChance;
    private float _suvChance;
    private float _busChance;
    private float _truckChance;

    private List<SplineWaypoint> originWaypoints = new List<SplineWaypoint>();

    // 0   <car  <suv   <bus   <truck        <no spawn>       span
    // |-----|-----|------|-------|----------------------------|
    private int carTreshold;
    private int suvTreshold;
    private int busTreshold;
    private int truckTreshold;

    private GameObject carPrefab;
    private GameObject suvPrefab;
    private GameObject busPrefab;
    private GameObject truckPrefab;

    public Material[] BodyMaterials;

    private Random rnd = new Random();

    private float multiplier = 1f;

    private int span;
    private float min;

    // Use this for initialization
    void Start()
    {
        // get all waypoints marked as origin
        var waypoints = Lanes.GetComponentsInChildren<SplineWaypoint>();
        foreach (var waypoint in waypoints)
        {
            if (waypoint.IsOrigin)
            {
                originWaypoints.Add(waypoint);
                //print(waypoint.name);
            }
        }

        // load prefabs from /Assets/Resources/<name>
        carPrefab = Resources.Load("Car", typeof(GameObject)) as GameObject;
        suvPrefab = Resources.Load("Suv", typeof(GameObject)) as GameObject;
        busPrefab = Resources.Load("Bus", typeof(GameObject)) as GameObject;
        truckPrefab = Resources.Load("Truck", typeof(GameObject)) as GameObject;

        UpdateThresholds();
    }


    /// <summary>
    /// Update thresholds for spawn chances
    /// </summary>
    private void UpdateThresholds()
    {
        // init spawn chances
        _carChance = CarSpawnChancePercentage/multiplier;
        _suvChance = SuvSpawnChancePercentage/multiplier;
        _busChance = BusSpawnChancePercentage/multiplier;
        _truckChance = TruckSpawnChancePercentage/multiplier;

        //print(string.Format("car={0}, suv={1}, bus={2}, truck={3}", _carChance, _suvChance, _busChance, _truckChance));

        // get min of all percentages
        min = Math.Min(_carChance,
            Math.Min(_suvChance,
                Math.Min(_busChance, _truckChance)));
        // get span
        span = ToInt(100/min);

        // get actual thresholds relative to span
        carTreshold = ToInt(span*(_carChance/100));
        suvTreshold = ToInt(span*(_suvChance/100) + carTreshold);
        busTreshold = ToInt(span*(_busChance/100) + suvTreshold);
        truckTreshold = ToInt(span*(_truckChance/100) + busTreshold);
    }



    // Update is called once per frame
    void Update()
    {
        // NO MORE, PLS NO MORE!!!
        if(Count == MaxVehicles)
            return;

        // get the prefab
        var prefab = GetRandomVehicle();
        if (prefab == null)
            return;

        // instanciate the prefab (spawn the vehicle)
        var car = Instantiate(prefab, transform) as GameObject;
        if(car == null)
            return;

        // one more vehicle...
        Count++;

        // set color
        var body = car.FindComponentInChildWithTag<Renderer>("Body");
        if (body)
            body.material = GetRandomMaterial();

        // set the origin of the instance
        var walker = car.GetComponent<SplineWalker>();
        walker.Waypoint = GetRandomOrigin();

        // attatch to parent
        walker.transform.parent = walker.Waypoint.Spline.transform;
    }


    /// <summary>
    /// Get a random vehicle
    /// </summary>
    /// <returns>prefab of vehicle</returns>
    private GameObject GetRandomVehicle()
    {
        //print("min=" + min + ", max=" + (span+1));
        var rand = rnd.Next(0, span + 1);

        if (rand < carTreshold)
            return carPrefab;
        if (rand < suvTreshold)
            return suvPrefab;
        if (rand < busTreshold)
            return busPrefab;
        if (rand < truckTreshold)
            return truckPrefab;

        return null;
    }


    /// <summary>
    /// Get a random origin spawn point
    /// </summary>
    /// <returns></returns>
    private SplineWaypoint GetRandomOrigin()
    {
        var rand = rnd.Next(0, originWaypoints.Count);
        return originWaypoints[rand];
    }


    private Material GetRandomMaterial()
    {
        var rand = rnd.Next(0, BodyMaterials.Length);
        return BodyMaterials[rand];
    }


    /// <summary>
    /// Convert float to int
    /// </summary>
    /// <param name="_float"></param>
    /// <returns></returns>
    private static int ToInt(float _float)
    {
        return (int) Math.Round(_float);
    }

    public void updateMultiplier(float value)
    {
        multiplier = value;
        UpdateThresholds();
    }
}