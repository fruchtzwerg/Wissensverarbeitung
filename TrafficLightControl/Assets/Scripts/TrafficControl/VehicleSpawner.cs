using System;
using System.Collections.Generic;
using System.Linq;
using UnityEngine;
using Random = System.Random;

public class VehicleSpawner : MonoBehaviour, IIntervalMultiplierUpdate, IProlog
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

    private static int _count;
    public static int Count
    {
        get { return _count; }
        set
        {
            _count = Math.Max(0, value);
        }
    }

    public Transform Lanes;
    public PrologWrapper Wrapper;

    private float _carChance;
    private float _suvChance;
    private float _busChance;
    private float _truckChance;

    private List<SplineWaypoint> originWaypoints = new List<SplineWaypoint>();
    private List<SplineWaypoint> destinationWaypoints = new List<SplineWaypoint>();

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

    public Material SpecialMaterial;
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
            else if (waypoint.IsDestination)
            {
                destinationWaypoints.Add(waypoint);
            }
        }

        // load prefabs from /Assets/Resources/<name>
        carPrefab = Resources.Load<GameObject>("Vehicles/Car");
        suvPrefab = Resources.Load<GameObject>("Vehicles/Suv");
        busPrefab = Resources.Load<GameObject>("Vehicles/Bus");
        truckPrefab = Resources.Load<GameObject>("Vehicles/Truck");

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
        if (Count >= MaxVehicles)
            return;

        Spawn();
    }


    /// <summary>
    /// Assigns a specified material to a car object.
    /// If no material is specified, a random one is chosen.
    /// </summary>
    /// <param name="car"></param>
    /// <param name="material"></param>
    private void Paint(GameObject car, Material material = null)
    {
        var body = car.FindComponentInChildWithTag<Renderer>("Body");
        material = material ?? GetRandomMaterial();

        if (body)
            body.material = material;
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
    /// Get a random origin spawn point.
    /// If the spawn point is already occupied with a vehicle,
    /// tries and gets a nother spawn point randomely.
    /// After three (3) unsuccessfull tries, null is returned.
    /// </summary>
    /// <returns>SplineWayoint origin, null after 3 tries</returns>
    private SplineWaypoint GetRandomOrigin()
    {
        for (var @try = 0; @try < 3; @try++)
        {
            // get random index
            var rand = rnd.Next(0, originWaypoints.Count);
            // get waypoint via index
            var origin = originWaypoints[rand];

            // if origin spawn point is NOT occupied
            // -> return valid spawn pont
            if (origin.GetComponents<SplineWaypoint>().All(o => !o.IsOccupied)) return origin;
        }

        // after 3rd try return null
        return null;
    }


    /// <summary>
    /// Get a random destination point
    /// </summary>
    /// <returns></returns>
    private SplineWaypoint GetRandomDestination()
    {
        var rand = rnd.Next(0, destinationWaypoints.Count);
        return destinationWaypoints[rand];
    }


    /// <summary>
    /// Gets a random material from specified array.
    /// </summary>
    /// <returns></returns>
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


    /// <summary>
    /// Interface callback to update the multiplier.
    /// (Used by pace slider)
    /// </summary>
    /// <param name="value"></param>
    public void updateMultiplier(float value)
    {
        multiplier = value;
        UpdateThresholds();
    }


    private void Spawn()
    {
        // get the prefab
        var prefab = GetRandomVehicle();
        if (prefab == null)
            return;

        var origin = GetRandomOrigin();
        if(origin == null)
            return;

        // instanciate the prefab (spawn the vehicle)
        var car = Instantiate(prefab, transform) as GameObject;
        if (car == null)
            return;

        // set color
        Paint(car);

        // set the origin of the instance
        var walker = car.GetComponent<SplineWalker>();
        walker.Waypoint = origin;

        // attatch to parent
        walker.transform.parent = walker.Waypoint.Spline.transform;

        // one more vehicle...
        Count++;
    }

    /// <summary>
    /// Spawns a vehicle at specified origin and destination.
    /// If no origin is specified -> fallback to random.
    /// If no destination is specified -> fallback to random.
    /// </summary>
    /// <param name="origin">origin wayopint as string</param>
    /// <param name="destination">destination waypoint as string</param>
    public void Spawn(string origin, string destination)
    {
        if (string.IsNullOrEmpty(origin))
            origin = GetRandomOrigin().name.ToLower();

        if (string.IsNullOrEmpty(destination))
            destination = GetRandomDestination().name.ToLower();

        var query = PrologWrapper.GetPath(origin, destination);
        Wrapper.QueryProlog(query, this);
    }


    /// <summary>
    /// Callback from IProlog Interface.
    /// Gets called when data is received from prolog.
    /// </summary>
    /// <param name="data"></param>
    public void ReceiveDataFromProlog(string data)
    {
        var task = UnityThreadHelper.Dispatcher.Dispatch(() => PrologWrapper.ParseAstarWaypoints(data));
        var result = task.Wait<SplineWaypoint[]>();
        if(result == null)
            return;

        UnityThreadHelper.Dispatcher.Dispatch(() => SpawnPostProlog(new Stack<SplineWaypoint>(result)));
    }


    /// <summary>
    /// Spawns a vehicle after the received data from prolog has been parsed
    /// and evaluated.
    /// </summary>
    /// <param name="waypoints"></param>
    private void SpawnPostProlog(Stack<SplineWaypoint> waypoints)
    {
        var vehicle = Instantiate(carPrefab);
        Paint(vehicle, SpecialMaterial);

        var walker = vehicle.GetComponent<SplineWalker>();
        walker.Waypoint = waypoints.Pop();
        walker.Waypoints = waypoints;
    }
}