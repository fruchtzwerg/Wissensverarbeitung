using UnityEngine;
using System.Collections;

public class WaitingObject
{
    public string Query { get; private set; }
    public IProlog Sender { get; private set; }


    public WaitingObject(string query, IProlog sender) {
        Query = query;
        Sender = sender;
    }
}
