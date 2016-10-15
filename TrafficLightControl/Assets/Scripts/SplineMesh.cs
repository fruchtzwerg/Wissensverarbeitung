using System;
using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using UnityEditor;

[ExecuteInEditMode]
public class SplineMesh : MonoBehaviour
{
    public BezierSpline Spline;
    public int Segments = 32;

    private void Start()
    {
        Mesh mesh = new Mesh();
        List<Vector3> vertices = new List<Vector3>();
        List<int> triangles = new List<int>();
        List<Vector3> normals = new List<Vector3>();

        Vector3 Start = Spline.GetPoint(0f);
        Quaternion rotation = Spline.GetRotation(0);
        Vector3 left = rotation * Vector3.left;
        Vector3 right = rotation * Vector3.right;
        Vector3 up = rotation * Vector3.up;
        vertices.Add(Start + right);
        vertices.Add(Start + left);
        normals.Add(up);
        normals.Add(up);
        int triIndex = 0;
        
        for (int i = 0; i <= Segments; i++)
        {
            float t = (float)i / (float)Segments;
            Vector3 End = Spline.GetPoint(t);
            rotation = Spline.GetRotation(t);

            left = rotation * Vector3.left;
            right = rotation * Vector3.right;
            up = rotation * Vector3.up;

            vertices.Add(End + right);
            vertices.Add(End + left);
            normals.Add(up);
            normals.Add(up);

            triangles.Add(triIndex);
            triangles.Add(triIndex + 1);
            triangles.Add(triIndex + 2);

            triangles.Add(triIndex + 2);
            triangles.Add(triIndex + 1);
            triangles.Add(triIndex + 3);

            triIndex += 2;

            Start = End;
        }

        mesh.SetVertices(vertices);
        mesh.SetNormals(normals);
        mesh.SetTriangles(triangles, 0);
        GetComponent<MeshFilter>().mesh = mesh;
    }

    // run Start() in scene edtor every frame
    void OnEnable() { EditorApplication.update += Start; }
    void OnDisable() { EditorApplication.update -= Start; }
}
