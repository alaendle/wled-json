using LanguageExt;
using LanguageExt.Traits;

public class State<F> {
    public required K<F, bool> On { get; set; }
    public required K<F, int> Bri { get; set; }
    public required K<F, int> Transition { get; set; }
    public required K<F, int> Ps { get; set; }
    public required K<F, int> Pl { get; set; }
    public required K<F, int> Lor { get; set; }
    public required K<F, int> Mainseg { get; set; }

    public override string ToString()
    {
        return "State: On:" + On + " Bri:" + Bri + " Transition:"+ Transition + " Ps:"+ Ps + " Pl:"+ Pl + " Lor:"+ Lor + " Mainseg:" + Mainseg;
    }
}

public static class StateExtensions {
    public static State<Option> Combine(this State<Option> state1, State<Option> state2) {
        return new State<Option> {
            On = state1.On.Combine(state2.On),
            Bri = state1.Bri.Combine(state2.Bri),
            Transition = state1.Transition.Combine(state2.Transition),
            Ps = state1.Ps.Combine(state2.Ps),
            Pl = state1.Pl.Combine(state2.Pl),
            Lor = state1.Lor.Combine(state2.Lor),
            Mainseg = state1.Mainseg.Combine(state2.Mainseg)
        };
    }
}