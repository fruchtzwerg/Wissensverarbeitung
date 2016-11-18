using System;
using System.Collections.Generic;
using UnityEngine;
using Random = System.Random;

public class TrainSpawner : MonoBehaviour
{
    public enum TrainDirection
    {
        Arrival,
        Departure,
        Both
    }

    private Random rng = new Random();
    private GameObject trainPrefab;

    public SplineWaypoint[] Origins;


    void Start()
    {
        trainPrefab = Resources.Load("Train/ICE") as GameObject;

        // spawn train for debugging
        SpawnTrain();
    }


    /// <summary>
    /// Spawns a train with a random direction
    /// </summary>
    public void SpawnTrain()
    {
        // random departure/arrival/both
        var direction = GetRandomDirection();
        // get waypoints accoarding to direction
        var origins = GetWaypoints(direction);

        // instanciate a train for each waypoint
        foreach (var waypoint in origins)
        {
            // instanciate the train
            var train = Instantiate(trainPrefab, waypoint.transform) as GameObject;
            train.GetComponent<SplineWalker>().Waypoint = waypoint;
        }
    }


    /// <summary>
    /// Gets origin waypoints accoarding to specified direction(s).
    /// </summary>
    /// <param name="direction"></param>
    /// <returns></returns>
    private IEnumerable<SplineWaypoint> GetWaypoints(TrainDirection direction)
    {
        switch (direction)
        {
            case TrainDirection.Arrival:
                return new[] {Origins[0]};
            case TrainDirection.Departure:
                return new[] {Origins[1]};
            case TrainDirection.Both:
                return Origins;
            default:
                throw new ArgumentOutOfRangeException("direction", direction, null);
        }
    }


    /// <summary>
    /// Returns a random direction
    /// </summary>
    /// <returns></returns>
    private TrainDirection GetRandomDirection()
    {
        // enum values to array
        var values = Enum.GetValues(typeof(TrainDirection));
        // random index from array
        var index = rng.Next(values.Length);

        return (TrainDirection) values.GetValue(index);
    }
}