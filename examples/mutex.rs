use petr_si::{
    render_dot_graph_image,
    Error,
    PetriNet,
    Place,
    State,
    Transition,
};
use std::{fs::File, io::BufWriter, process};

fn try_main() -> Result<(), Error> {
    let mut net = PetriNet::new();
    let p1 = net.add_place(Place::new("P1"))?;
    let p2 = net.add_place(Place::new("P2"))?;
    let p3 = net.add_place(Place::new("P3"))?;
    let p4 = net.add_place(Place::new("P4"))?;
    let s = net.add_place(Place::new("S"))?;
    let _t1 = net.add_transition(Transition::new("t1", [p1, s], [p2]))?;
    let _t2 = net.add_transition(Transition::new("t2", [p2], [p1, s]))?;
    let _t3 = net.add_transition(Transition::new("t3", [s, p3], [p4]))?;
    let _t4 = net.add_transition(Transition::new("t4", [p4], [s, p3]))?;
    let state = State { tokens: vec![1, 0, 1, 0, 1] };
    render_dot_graph_image(
        &net,
        &state,
        BufWriter::new(File::create("mutex.svg")?),
    )?;
    Ok(())
}

fn main() {
    if let Err(error) = try_main() {
        eprintln!("{}", error);
        process::exit(1);
    }
}
