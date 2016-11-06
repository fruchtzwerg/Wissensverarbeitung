﻿using UnityEditor;
using UnityEngine;

[ExecuteInEditMode]
[CustomEditor(typeof(SplineModifier))]
public class SplineModifierInspctor : Editor
{
    public BezierSpline[] splines;
    private const int stepsPerCurve = 10;
    private const float directionScale = 0.5f;
    private const float handleSize = 0.04f;
    private const float pickSize = 0.06f;
    private static readonly Color[] ModeColors = {
        Color.white,
        Color.yellow,
        Color.cyan
    };
    int selectedIndex = -1;

    private void OnSceneGUI()
    {
        splines = (target as SplineModifier).GetComponentsInChildren<BezierSpline>();

        foreach(var spline in splines)
        {
            var handleTransform = spline.transform;
            var handleRotation = Tools.pivotRotation == PivotRotation.Local ?
                handleTransform.rotation : Quaternion.identity;

            Vector3 p0 = ShowPoint(0, handleTransform, spline, handleRotation);
            for (int i = 1; i < spline.ControlPointCount; i += 3)
            {
                Vector3 p1 = ShowPoint(i, handleTransform, spline, handleRotation);
                Vector3 p2 = ShowPoint(i + 1, handleTransform, spline, handleRotation);
                Vector3 p3 = ShowPoint(i + 2, handleTransform, spline, handleRotation);

                Handles.color = Color.gray;
                Handles.DrawLine(p0, p1);
                Handles.DrawLine(p2, p3);

                Handles.DrawBezier(p0, p3, p1, p2, Color.magenta, null, 2f);
                p0 = p3;
            }
            ShowDirections(spline);
        }
    }


    public void OnInspectorGUI(BezierSpline spline)
    {
        EditorGUI.BeginChangeCheck();
        bool loop = EditorGUILayout.Toggle("Loop", spline.Loop);
        Transform startTransform = (Transform)EditorGUILayout.ObjectField("Start Point", spline.StartPoint, typeof(Transform), true);
        Transform endTransform = (Transform)EditorGUILayout.ObjectField("End Point", spline.EndPoint, typeof(Transform), true);
        if (EditorGUI.EndChangeCheck())
        {
            Undo.RecordObject(spline, "Toggle Loop");
            Undo.RecordObject(spline, "Select Start Point");
            Undo.RecordObject(spline, "Select End Point");
            EditorUtility.SetDirty(spline);
            spline.Loop = loop;
            spline.StartPoint = startTransform;
            spline.EndPoint = endTransform;
        }
        if (selectedIndex >= 0 && selectedIndex < spline.ControlPointCount)
        {
            DrawSelectedPointInspector(spline);
        }
        if (GUILayout.Button("Add Curve"))
        {
            Undo.RecordObject(spline, "Add Curve");
            spline.AddCurve();
            EditorUtility.SetDirty(spline);
        }
    }

    private void DrawSelectedPointInspector(BezierSpline spline)
    {
        GUILayout.Label("Selected Point");
        EditorGUI.BeginChangeCheck();
        Vector3 point = EditorGUILayout.Vector3Field("Position", spline.GetControlPoint(selectedIndex));
        if (EditorGUI.EndChangeCheck())
        {
            Undo.RecordObject(spline, "Move Point");
            EditorUtility.SetDirty(spline);
            spline.SetControlPoint(selectedIndex, point);
        }
        EditorGUI.BeginChangeCheck();
        BezierControlPointMode mode = (BezierControlPointMode)
            EditorGUILayout.EnumPopup("Mode", spline.GetControlPointMode(selectedIndex));
        if (EditorGUI.EndChangeCheck())
        {
            Undo.RecordObject(spline, "Change Point Mode");
            spline.SetControlPointMode(selectedIndex, mode);
            EditorUtility.SetDirty(spline);
        }
    }


    private Vector3 ShowPoint(int index, Transform handleTransform, BezierSpline spline, Quaternion handleRotation)
    {
        Vector3 point = handleTransform.TransformPoint(spline.GetControlPoint(index));
        float size = HandleUtility.GetHandleSize(point);
        if (index == 0)
        {
            size *= 2f;
        }
        Handles.color = ModeColors[(int)spline.GetControlPointMode(index)];
        if (Handles.Button(point, handleRotation, size * handleSize, size * pickSize, Handles.DotCap))
        {
            selectedIndex = index;
            Repaint();
        }
        if (selectedIndex == index)
        {
            EditorGUI.BeginChangeCheck();
            point = Handles.DoPositionHandle(point, handleRotation);
            if (EditorGUI.EndChangeCheck())
            {
                Undo.RecordObject(spline, "Move Point");
                EditorUtility.SetDirty(spline);
                spline.SetControlPoint(index, handleTransform.InverseTransformPoint(point));
            }
        }
        return point;
    }


    private void ShowDirections(BezierSpline spline)
    {
        Handles.color = Color.green;
        Vector3 point = spline.GetPoint(0f);
        Handles.DrawLine(point, point + spline.GetDirection(0f) * directionScale);
        int steps = stepsPerCurve * spline.CurveCount;
        for (int i = 1; i <= steps; i++)
        {
            point = spline.GetPoint(i / (float)steps);
            Handles.DrawLine(point, point + spline.GetDirection(i / (float)steps) * directionScale);
        }
    }
}
