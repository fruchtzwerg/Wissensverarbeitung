using UnityEngine;
using System.Text;
using System;
using System.Linq;

public class AStar : MonoBehaviour
{

    private const string ARC_PRE = "arc(";
    private const string ARC_POST = "),";
    private const string SEP = ",";
    private const string TAG_LANES = "Lanes";
    private readonly StringBuilder _sb = new StringBuilder();
    public int DistanceMultiplier = 10;

    public string BuildAStarTreeString()
    {
        //clear stringbuilder
        _sb.Remove(0, _sb.Length);

        _sb.Append("[");

        //CreateItemsStatic();
        CreateItemsDynamic();
        _sb.Remove(_sb.Length - 1, 1);
        _sb.Append("]");

        print(_sb.ToString().ToLower());
        return _sb.ToString().ToLower();
    }


    /// <summary>
    /// Dynamically creates a tree from waypoints.
    /// Takes into account all SplineWaypoint children
    /// of any parent marked with a "Lanes" tag.
    /// </summary>
    private void CreateItemsDynamic()
    {
        // get parent of lanes
        var lanesParents = GameObject.FindGameObjectsWithTag(TAG_LANES);
        // get all waypoints marked as IsOrigin
        var origins = (from parent in lanesParents
            from waypoint in parent.GetComponentsInChildren<SplineWaypoint>()
            where waypoint.IsOrigin
            select waypoint).ToArray();

        // build an arc of the tree for each origin
        foreach (var origin in origins)
            BuildArc(origin);
    }


    /// <summary>
    /// Recursively builds an arc of the waypoint tree
    /// from a given origin.
    /// </summary>
    /// <param name="waypoint"></param>
    private void BuildArc(SplineWaypoint waypoint)
    {
        // get all waypoints registered to the waypoint gameObject
        var origins = waypoint.GetComponents<SplineWaypoint>();

        // for each of these waypoints...
        foreach (var origin in origins)
        {
            // break condition:
            // done, if this is a destination or there is no next waypoint
            if (origin.IsDestination || origin.NextWaypoint == null)
                return;

            var sb = new StringBuilder();

            // (
            sb.Append(ARC_PRE);
            // (origin.name,
            sb.Append(origin.name).Append(SEP);
            // (origin.name,next.name,
            sb.Append(origin.NextWaypoint.name).Append(SEP);

            // calculate costs 
            //var numVehicles = origin.GetComponentsInChildren<SplineWalker>().Length;
            // constant costs
            // (origin.name,next.name,5,
            sb.Append(5).Append(SEP);

            // (origin.name,next.name,5,distance
            sb.Append(CalculteDistance(origin.transform, origin.NextWaypoint.transform));
            // (origin.name,next.name,5,distance),
            sb.Append(ARC_POST);

            // do not append duplicates to tree
            if (!_sb.ToString().Contains(sb.ToString()))
                _sb.Append(sb);

            // recursive call for next waypoint
            BuildArc(origin.NextWaypoint);
        }
    }

    private int CalculteDistance(Transform start, Transform followNode)
    {
        var distance = Vector3.Distance(start.position, followNode.position)*DistanceMultiplier;
        return (int) Math.Round(distance);
    }
}