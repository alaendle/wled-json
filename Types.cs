class State {
    public bool On { get; set; }
    public int Bri { get; set; }
    public int Transition { get; set; }
    public int Ps { get; set; }
    public int Pl { get; set; }
    public int Lor { get; set; }
    public int Mainseg { get; set; }

    public override string ToString()
    {
        return "State: On:" + On + " Bri:" + Bri + " Transition:"+ Transition + " Ps:"+ Ps + " Pl:"+ Pl + " Lor:"+ Lor + " Mainseg:" + Mainseg;
    }
}

class StatePatch {
    public Nullable<bool> On { get; set; }
    public Nullable<int> Bri { get; set; }
    public Nullable<int> Transition { get; set; }
    public Nullable<int> Ps { get; set; }
    public Nullable<int> Pl { get; set; }
    public Nullable<int> Lor { get; set; }
    public Nullable<int> Mainseg { get; set; }

    internal static StatePatch Merge(StatePatch patch0, StatePatch patch1)
    {
        var res = new StatePatch
        {
            On = patch0.On.HasValue ? patch0.On : patch1.On,
            Bri= patch0.Bri.HasValue ? patch0.Bri : patch1.Bri,
            Transition= patch0.Transition.HasValue ? patch0.Transition : patch1.Transition,
            Ps= patch0.Ps.HasValue ? patch0.Ps : patch1.Ps,
            Pl= patch0.Pl.HasValue ? patch0.Pl : patch1.Pl,
            Lor= patch0.Lor.HasValue ? patch0.Lor : patch1.Lor,
            Mainseg= patch0.Mainseg.HasValue ? patch0.Mainseg : patch1.Mainseg
        };
        return res;
    }

    public override string ToString()
    {
        return "State: On:" + On + " Bri:" + Bri + " Transition:"+ Transition + " Ps:"+ Ps + " Pl:"+ Pl + " Lor:"+ Lor + " Mainseg:" + Mainseg;
    }
}