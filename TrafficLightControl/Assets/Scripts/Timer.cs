using System;


public class Timer
{
    public float Interval = 1000;
    public float Remaining = 1000;
    public bool AutoReset = true;
    public bool Enabled = true;

    public void Update(float deltaTime)
    {
        if (!Enabled) return;

        Remaining -= deltaTime * 1000;
        if (Remaining <= 0)
        {
            OnElapsed();

            if (AutoReset) Reset();
            else Enabled = false;
        }
    }

    public void Start()
    {
        Reset();
        Enabled = true;
    }

    public void Stop()
    {
        Enabled = false;
    }

    public void Reset()
    {
        Remaining = Interval;
    }

    public event EventHandler Elapsed;

    private void OnElapsed()
    {
        var handler = Elapsed;

        if (handler != null)
        {
            handler(this, null);
        }
    }
}