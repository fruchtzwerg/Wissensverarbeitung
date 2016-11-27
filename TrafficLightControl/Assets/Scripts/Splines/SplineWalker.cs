using System.Collections.Generic;
using UnityEngine;
using Random = System.Random;

public class SplineWalker : MonoBehaviour
{
    public enum LookDirection
    {
        None,
        Forward,
        Backward,
        Left,
        Right,
        Up,
        Down
    }


    public enum SplineWalkerMode
    {
        Once,
        Loop,
        PingPong,
        Connected
    }


    public SplineWaypoint Waypoint;
    public Stack<SplineWaypoint> Waypoints;
    public float HeightOffset = 0.5f;
    public float Duration = 3;
    public LookDirection LookAtDirection;
    public SplineWalkerMode Mode = SplineWalkerMode.Connected;

    private float _progress;
    private BezierSpline _spline;
    private bool _isGoingForward = true;

    public bool Move = true;

    public bool IsTrain;

    private readonly Random _rng = new Random();
    private bool destroy;


    private void FixedUpdate()
    {
        // due to collider mechanics we have to move
        // objects to trigger OnTriggerExit() before
        // destroying them.
        // any object below y=-50 should be destroyed.
        if (destroy)
            Destroy();

        // get the spline to walk on
        _spline = Waypoint.Spline;


        // move
        if (_isGoingForward)
        {
            MoveForward();
        }
        else
        {
            MoveBackward();
        }

        // actually move the model
        var position = _spline.GetPoint(_progress) + Vector3.up*HeightOffset;
        transform.position = position;

        // if no look direction is specified, no need to go further
        if (LookAtDirection == LookDirection.None)
            return;

        // else turn model
        LookAt(position);
    }

    private void Destroy()
    {
        if (IsTrain) return;

        VehicleSpawner.Count--;
        Destroy(gameObject);
    }


    /// <summary>
    /// Turn Model at position in a defined direction.
    /// </summary>
    /// <param name="position">Model's position</param>
    private void LookAt(Vector3 position)
    {
        // turn the model to face forward (0,0,-1)
        transform.LookAt(position + _spline.GetDirection(_progress));

        // truck model is flipped, so flip it back
        if (name.ToLower().Contains("truck"))
            transform.localEulerAngles += new Vector3(-90, 0, 0);

        // turn the model in a specific direction
        switch (LookAtDirection)
        {
            case LookDirection.Forward:
                break;
            case LookDirection.Backward:
                transform.localEulerAngles += new Vector3(0, 180, 0);
                break;
            case LookDirection.Left:
                transform.localEulerAngles += new Vector3(0, 90, 0);
                break;
            case LookDirection.Right:
                transform.localEulerAngles += new Vector3(0, -90, 0);
                break;
            case LookDirection.Up:
                transform.localEulerAngles += new Vector3(0, 0, 90);
                break;
            case LookDirection.Down:
                transform.localEulerAngles += new Vector3(0, 0, -90);
                break;
        }
    }


    /// <summary>
    /// Move model in forward direction.
    /// </summary>
    /// <returns>true: model was destroyed or stoped, else false</returns>
    private void MoveForward()
    {
        //StopMoving();
        // stop movement if neccesary
        if (!Move) return;

        // move
        _progress += (Time.deltaTime/Duration)*_spline.Speed;

        // not at the end of a node -> done
        if (_progress < 1f) return;

        // account for the selected mode
        switch (Mode)
        {
            case SplineWalkerMode.Once:
                _progress = 1f;
                break;
            case SplineWalkerMode.Loop:
                _progress -= 1f;
                break;
            case SplineWalkerMode.Connected:
                if (Waypoint.NextWaypoint.IsDestination)
                {
                    // moving the vehicle out of the way before destroying it
                    // is necessary because on high speeds it is possible
                    // for following vehicles to hit this car's collider and stop.
                    // once the this vehicle is destroyed, the follower will remain
                    // stuck because OnTriggerExit() is never fired.
                    // because of this we move the vehicle below y = -50 and check
                    // in the update if this vehicle is below that threshold,
                    // then destroy it.
                    transform.Translate(Vector3.down * 1000);
                    destroy = true;
                    return;
                }

                // get a the next waypoint randomly
                if (Waypoints == null)
                {
                    Waypoint = GetRandomWaypoint(Waypoint.NextWaypoint);
                }
                else
                {
                    Waypoint = Waypoints.Pop();
                }

                _spline = Waypoint.Spline;
                transform.parent = _spline.transform;
                _progress = 0;
                break;
            default:
                _progress = 2f - _progress;
                _isGoingForward = false;
                break;
        }
        return;
    }


    /// <summary>
    /// Move the model in backwards direction.
    /// </summary>
    private void MoveBackward()
    {
        _progress -= (Time.deltaTime/Duration)*_spline.Speed;

        if (_progress > 0f) return;

        _progress = -_progress;
        _isGoingForward = true;
    }


    /// <summary>
    /// Get a random origin spawn point
    /// </summary>
    /// <returns></returns>
    private SplineWaypoint GetRandomWaypoint(SplineWaypoint waypoint)
    {
        // get waypoints registered with this waypoint
        var waypoints = waypoint.GetComponents<SplineWaypoint>();
        var weights = new int[waypoints.Length];
        var sum = 0;

        // get weights
        for (var i = 0; i < waypoints.Length; i++)
        {
            sum += waypoints[i].Weight;
            weights[i] = sum;
        }

        // get random index of waypoint
        var rand = _rng.Next(1, sum + 1);

        for (var i = 0; i < weights.Length; i++)
        {
            if (rand <= weights[i])
                return waypoints[i];
        }

        return waypoints[0];
    }
}