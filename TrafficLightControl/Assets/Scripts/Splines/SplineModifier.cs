using UnityEngine;
using UnityEditor;


[ExecuteInEditMode]
public class SplineModifier : MonoBehaviour
{

    private BezierSpline[] splines;

    void ShowSplines()
    {
        splines = GetComponentsInChildren<BezierSpline>();

        foreach (var spline in splines)
        {
            spline.UpdatePositionsInEditor();
        }
    }



    // update in editor
    void OnEnable() { EditorApplication.update += ShowSplines; }
    void OnDisable() { EditorApplication.update -= ShowSplines; }
}
