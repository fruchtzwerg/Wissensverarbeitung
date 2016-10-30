using UnityEngine;
using System.Collections;

public class WaitingObject{

    private string query;
    private IProlog sender;



    public WaitingObject(string query, IProlog sender) {
        this.query = query;
        this.sender = sender;
    }

    public string Query {
        get {return query;}
    }

    public IProlog Sender {
        get { return sender; }        
    }
}
