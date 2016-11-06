using System;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;
using Random = System.Random;

public class VehicleSpawner : MonoBehaviour
{
    // spwan chances in percentage per frame
    // 1f = 1% chance each frame
    // @60 fps = 60% chance each second
    // = 1 car each 100 seconds on average
    public float CarSpawnChancePercentage = 5f;
    public float SuvSpawnChancePercentage = 1.5f;
    public float BusSpawnChancePercentage = .1f;
    public float TruckSpawnChancePercentage = .5f;

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

    private Random rnd = new Random();

    private int span;
    private float min;

    // Use this for initialization
    void Start()
    {
        // get all waypoints marked as origin
        var waypoints = GameObject.Find("Lanes").GetComponentsInChildren<SplineWaypoint>();
        foreach (var waypoint in waypoints)
        {
            if (waypoint.IsOrigin)
            {
                originWaypoints.Add(waypoint);
                print(waypoint.name);
            }
        }

        // load prefabs from /Assets/Resources/<name>
        carPrefab = Resources.Load("Car", typeof(GameObject)) as GameObject;
        suvPrefab = Resources.Load("Suv", typeof(GameObject)) as GameObject;
        busPrefab = Resources.Load("Bus", typeof(GameObject)) as GameObject;
        truckPrefab = Resources.Load("Truck", typeof(GameObject)) as GameObject;

        // get min of all percentages
        min = Math.Min(CarSpawnChancePercentage,
            Math.Min(SuvSpawnChancePercentage,
                Math.Min(BusSpawnChancePercentage, TruckSpawnChancePercentage)));
        // get span
        span = ToInt(100/min);

        // get actual thresholds relative to span
        carTreshold = ToInt(span*(CarSpawnChancePercentage/100));
        suvTreshold = ToInt(span*(SuvSpawnChancePercentage/100) + carTreshold);
        busTreshold = ToInt(span*(BusSpawnChancePercentage/100) + suvTreshold);
        truckTreshold = ToInt(span*(TruckSpawnChancePercentage/100) + busTreshold);
    }

    // Update is called once per frame
    void Update()
    {
        // get the prefab
        GameObject prefab = GetRandomVehicle();
        if (prefab == null)
            return;

        // instanciate the prefab
        var car = Instantiate(prefab, transform) as GameObject;

        // set the origin of the instance
        var walker = car.GetComponent<SplineWalker>();
        walker.Waypoint = GetRandomOrigin();
    }


    /// <summary>
    /// Get a random vehicle
    /// </summary>
    /// <returns>prefab of vehicle</returns>
    private GameObject GetRandomVehicle()
    {
        int rand = rnd.Next(0, span + 1);

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
        int rand = rnd.Next(0, originWaypoints.Count);

        return originWaypoints[rand];
    }


    /// <summary>
    /// Convert float to int
    /// </summary>
    /// <param name="_float"></param>
    /// <returns></returns>
    private int ToInt(float _float)
    {
        return (int) Math.Round(_float);
    }
}