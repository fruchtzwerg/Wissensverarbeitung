
using UnityEngine;
using System.Collections.Generic;
using UnityEditor;

[ExecuteInEditMode]
public class SplineMesh : MonoBehaviour
{
    public BezierSpline Spline;
    public int Segments = 64;
    public float Height = 0.1f;

    private void Start()
    {
        Mesh mesh = new Mesh();
        List<Vector3> vertices = new List<Vector3>();
        List<int> triangles = new List<int>();
        List<Vector3> normals = new List<Vector3>();
        List<Vector2> uvs = new List<Vector2>();

        Vector3 start = Spline.GetPoint(0f) - transform.position;
        Quaternion rotation = Spline.GetRotation(0);
        //Vector3 left = rotation * Vector3.left;
        //Vector3 right = rotation * Vector3.right;
        Vector3 segmentRight = Vector3.Cross(Vector3.up, Spline.GetDirection(0f));
        Vector3 left = -segmentRight;
        Vector3 right = segmentRight;
        Vector3 up = rotation * Vector3.up;
        vertices.Add(start + right);
        normals.Add(up);
        vertices.Add(start + right + Vector3.down * Height);
        normals.Add(up);
        vertices.Add(start + left);
        normals.Add(up);
        vertices.Add(start + left + Vector3.down * Height);
        normals.Add(up);
        uvs.Add(new Vector2(1, 0));
        uvs.Add(new Vector2(0, 0));
        uvs.Add(new Vector2(1, 0));
        uvs.Add(new Vector2(0, 0));

        for (int i = 0; i <= Segments; i++)
        {
            float t = (float)i / Segments;
            Vector3 point = Spline.GetPoint(t) - transform.position;
            rotation = Spline.GetRotation(t);
            segmentRight = Vector3.Cross(Vector3.up, Spline.GetDirection(t));

            //left = rotation * Vector3.left;
            //right = rotation * Vector3.right;
            left = -segmentRight;
            right = segmentRight;

            var triIndex = vertices.Count-4;
            Vector3[] verts = new[]
            {
                point + right,
                point + right + Vector3.down*Height,
                point + left,
                point + left + Vector3.down*Height
            };
            vertices.AddRange(verts);

            Vector3[] norms = new[]
            {
                up, up, up, up
            };
            normals.AddRange(norms);

            Vector2[] uvVect;
            if (i%2 == 0)
            {
                uvVect = new[]
                {
                    new Vector2(1, 1),
                    new Vector2(1, 1),
                    new Vector2(0, 1),
                    new Vector2(0, 1)
                };
            }
            else
            {
                uvVect = new[]
                {
                    new Vector2(1, 0),
                    new Vector2(1, 0),
                    new Vector2(0, 0),
                    new Vector2(0, 0)
                };
            }
            uvs.AddRange(uvVect);

            //var triIndex = i*2;
            //triangles.Add(triIndex);
            //triangles.Add(triIndex + 1);
            //triangles.Add(triIndex + 2);

            //triangles.Add(triIndex + 2);
            //triangles.Add(triIndex + 1);
            //triangles.Add(triIndex + 3);

            int[] segmentTriangles = 
            {
                triIndex + 0, triIndex + 2, triIndex + 4, //top face
                triIndex + 2, triIndex + 6, triIndex + 4,
                triIndex + 0, triIndex + 5, triIndex + 1, //right face
                triIndex + 0, triIndex + 4, triIndex + 5,
                triIndex + 1, triIndex + 3, triIndex + 5, //bottom face
                triIndex + 3, triIndex + 7, triIndex + 5,
                triIndex + 2, triIndex + 3, triIndex + 7, //left face
                triIndex + 2, triIndex + 7, triIndex + 6
            };
            triangles.AddRange(segmentTriangles);
        }

        mesh.SetVertices(vertices);
        mesh.SetNormals(normals);
        mesh.SetTriangles(triangles, 0);
        mesh.SetUVs(0, uvs);
        mesh.uv = uvs.ToArray();
        if(!GetComponent<MeshFilter>())
            gameObject.AddComponent<MeshFilter>();
        if (!GetComponent<MeshRenderer>())
            gameObject.AddComponent<MeshRenderer>();
        GetComponent<MeshFilter>().mesh = mesh;

        //mesh.RecalculateNormals();
        mesh.RecalculateBounds();
        mesh.Optimize();

    }

    // run Start() in scene edtor every frame
    void OnEnable() { EditorApplication.update += Start; }
    void OnDisable() { EditorApplication.update -= Start; }

    //void Update()
    //{
    //    if (Input.GetKeyDown("up"))
    //        Start();
    //}
}
