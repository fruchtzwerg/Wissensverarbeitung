using UnityEngine;
#if UNITY_EDITOR
using UnityEditor;
#endif


[ExecuteInEditMode]
public class SplineModifier : MonoBehaviour
{

    private BezierSpline[] splines;
    private MeshRenderer[] waypoints;

    void ShowSplines()
    {
        splines = GetComponentsInChildren<BezierSpline>();

        foreach (var spline in splines)
        {
            spline.UpdatePositionsInEditor();
        }
    }

    void Start()
    {
        waypoints = GetComponentsInChildren<MeshRenderer>();

        foreach (var waypoint in waypoints)
        {
            waypoint.enabled = false;
        }
    }

    void OnDestroy()
    {
        foreach (var waypoint in waypoints)
        {
            waypoint.enabled = true;
        }
    }

#if UNITY_EDITOR
    // update in editor
    void OnEnable() { EditorApplication.update += ShowSplines; }
    void OnDisable() { EditorApplication.update -= ShowSplines; }
#endif
}
