use std::{
    cmp::Ordering,
    collections::{hash_map, BTreeSet, HashMap},
    fmt,
    hash::{Hash, Hasher},
    io::{self, Read},
    process::{Command, Stdio},
    slice,
    sync::Arc,
};

use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("place {} already exists", .0)]
    PlaceAlreadyExists(Arc<str>),
    #[error("place handle {} is invalid", .0.index)]
    InvalidPlaceHandle(PlaceHandle),
    #[error("transition {} already exists", .0)]
    TransitionAlreadyExists(Arc<str>),
    #[error("transition handle {} is invalid", .0.index)]
    InvalidTransitionHandle(TransitionHandle),
    #[error("reachabilty graph node with state {} already exists", .0)]
    NodeAlreadyExists(State),
    #[error("petri net not compatible with state {}", .0)]
    NetNotCompatible(State),
    #[error("error displaying formatted data")]
    Fmt(#[from] fmt::Error),
    #[error(transparent)]
    Io(#[from] io::Error),
    #[error("graphivz failed: {}", .0)]
    GraphVizFailed(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PlaceHandle {
    pub index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TransitionHandle {
    pub index: usize,
}

#[derive(Debug, Clone, Default)]
pub struct PetriNet {
    places: Vec<Place>,
    places_by_ident: HashMap<Arc<str>, PlaceHandle>,
    transitions: Vec<Transition>,
    transitions_by_ident: HashMap<Arc<str>, TransitionHandle>,
}

impl PartialEq for PetriNet {
    fn eq(&self, other: &Self) -> bool {
        self.places == other.places && self.transitions == other.transitions
    }
}

impl Eq for PetriNet {}

impl PartialOrd for PetriNet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PetriNet {
    fn cmp(&self, other: &Self) -> Ordering {
        self.places
            .cmp(&other.places)
            .then_with(|| self.transitions.cmp(&other.transitions))
    }
}

impl Hash for PetriNet {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        self.places.hash(state);
        self.transitions.hash(state);
    }
}

impl PetriNet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_place(&mut self, place: Place) -> Result<PlaceHandle, Error> {
        match self.places_by_ident.entry(place.label.clone()) {
            hash_map::Entry::Occupied(_) => {
                Err(Error::PlaceAlreadyExists(place.label.clone()))
            },

            hash_map::Entry::Vacant(entry) => {
                let handle = PlaceHandle { index: self.places.len() };

                self.places.push(place);
                Ok(*entry.insert(handle))
            },
        }
    }

    pub fn get_place(&self, handle: PlaceHandle) -> Result<&Place, Error> {
        self.places.get(handle.index).ok_or(Error::InvalidPlaceHandle(handle))
    }

    pub fn places(&self) -> Places {
        Places { index: 0, inner: self.places.iter() }
    }

    pub fn add_transition(
        &mut self,
        transition: Transition,
    ) -> Result<TransitionHandle, Error> {
        for &handle in &transition.inputs {
            self.get_place(handle)?;
        }

        for &handle in &transition.outputs {
            self.get_place(handle)?;
        }

        match self.transitions_by_ident.entry(transition.label.clone()) {
            hash_map::Entry::Occupied(_) => {
                Err(Error::TransitionAlreadyExists(transition.label.clone()))?
            },

            hash_map::Entry::Vacant(entry) => {
                let handle = TransitionHandle { index: self.transitions.len() };
                self.transitions.push(transition);
                entry.insert(handle);
                Ok(handle)
            },
        }
    }

    pub fn get_transition(
        &self,
        handle: TransitionHandle,
    ) -> Result<&Transition, Error> {
        self.transitions
            .get(handle.index)
            .ok_or(Error::InvalidTransitionHandle(handle))
    }

    pub fn transitions(&self) -> Transitions {
        Transitions { index: 0, inner: self.transitions.iter() }
    }

    pub fn is_enabled(
        &self,
        handle: TransitionHandle,
        state: &State,
    ) -> Result<bool, Error> {
        let is_enabled = self
            .get_transition(handle)?
            .inputs
            .iter()
            .any(|input_handle| state.tokens[input_handle.index] > 0);

        Ok(is_enabled)
    }
}

#[derive(Debug, Clone)]
pub struct Places<'a> {
    index: usize,
    inner: slice::Iter<'a, Place>,
}

impl<'a> Iterator for Places<'a> {
    type Item = (PlaceHandle, &'a Place);

    fn next(&mut self) -> Option<Self::Item> {
        let place = self.inner.next()?;
        let handle = PlaceHandle { index: self.index };
        self.index += 1;
        Some((handle, place))
    }
}

#[derive(Debug, Clone)]
pub struct Transitions<'a> {
    index: usize,
    inner: slice::Iter<'a, Transition>,
}

impl<'a> Iterator for Transitions<'a> {
    type Item = (TransitionHandle, &'a Transition);

    fn next(&mut self) -> Option<Self::Item> {
        let transition = self.inner.next()?;
        let handle = TransitionHandle { index: self.index };
        self.index += 1;
        Some((handle, transition))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Place {
    pub label: Arc<str>,
}

impl Place {
    pub fn new<S>(label: S) -> Self
    where
        S: Into<Arc<str>>,
    {
        Self { label: label.into() }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Transition {
    pub label: Arc<str>,
    pub inputs: BTreeSet<PlaceHandle>,
    pub outputs: BTreeSet<PlaceHandle>,
}

impl Transition {
    pub fn new<S, I, O>(label: S, inputs: I, outputs: O) -> Self
    where
        S: Into<Arc<str>>,
        I: IntoIterator<Item = PlaceHandle>,
        O: IntoIterator<Item = PlaceHandle>,
    {
        Self {
            label: label.into(),
            inputs: inputs.into_iter().collect(),
            outputs: outputs.into_iter().collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct State {
    pub tokens: Vec<u64>,
}

impl State {
    pub fn display_named<W>(
        &self,
        net: &PetriNet,
        mut dest: W,
    ) -> Result<(), Error>
    where
        W: fmt::Write,
    {
        if net.places.len() != self.tokens.len() {
            Err(Error::NetNotCompatible(self.clone()))?;
        }

        for ((handle, place), token_count) in
            net.places().zip(self.tokens.iter())
        {
            if handle.index > 0 {
                write!(dest, ", ")?;
            }
            write!(dest, "{}: {}", place.label, token_count)?;
        }

        Ok(())
    }
}

impl fmt::Display for State {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        for (i, token_count) in self.tokens.iter().enumerate() {
            if i > 0 {
                write!(fmtr, ", ")?;
            }
            write!(fmtr, "{}", token_count)?;
        }
        Ok(())
    }
}

fn render_string<W>(string: &str, mut dest: W) -> Result<(), Error>
where
    W: io::Write,
{
    write!(dest, "\"")?;
    for ch in string.chars() {
        if ch == '"' || ch == '\\' {
            write!(dest, "\\")?;
        }
        write!(dest, "{}", ch)?;
    }
    write!(dest, "\"")?;
    Ok(())
}

pub fn render_dot_graph_source<W>(
    net: &PetriNet,
    state: &State,
    mut dest: W,
) -> Result<(), Error>
where
    W: io::Write,
{
    if net.places.len() != state.tokens.len() {
        Err(Error::NetNotCompatible(state.clone()))?;
    }

    write!(dest, "digraph {{ ")?;
    for (handle, place) in net.places() {
        write!(dest, "p{}[shape=\"circle\", xlabel=", handle.index)?;
        render_string(&place.label, &mut dest)?;
        write!(dest, " label=\"")?;
        for i in 0 .. state.tokens[handle.index] {
            if i > 0 {
                write!(dest, " ")?;
            }
            write!(dest, "⬤")?;
        }
        write!(dest, "\"]; ")?;
    }
    for (handle, transition) in net.transitions() {
        write!(
            dest,
            "t{}[shape=\"rectangle\", height=0.25, style=\"filled\", \
             color=\"black\", label=\"\", xlabel=",
            handle.index
        )?;
        render_string(&transition.label, &mut dest)?;
        write!(dest, "]; ")?;
        for input in &transition.inputs {
            write!(dest, "p{} -> t{}; ", input.index, handle.index)?;
        }
        for output in &transition.outputs {
            write!(dest, "t{} -> p{}; ", handle.index, output.index)?;
        }
    }
    write!(dest, "}}")?;
    Ok(())
}

pub fn render_dot_graph_image<W>(
    net: &PetriNet,
    state: &State,
    dest: W,
) -> Result<(), Error>
where
    W: io::Write,
{
    render_dot_graph_image_with_cmd(net, state, dest, &mut Command::new("dot"))
}

pub fn render_dot_graph_image_with_cmd<W>(
    net: &PetriNet,
    state: &State,
    mut dest: W,
    command: &mut Command,
) -> Result<(), Error>
where
    W: io::Write,
{
    let mut process = command
        .arg("-Tsvg")
        .stdout(Stdio::piped())
        .stdin(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;
    {
        let mut stdin = process.stdin.take().expect("process had stdin piped");
        render_dot_graph_source(net, state, &mut stdin)?;
    }

    let mut stdout = process.stdout.take().expect("process had stdout piped");
    let mut stderr = process.stderr.take().expect("process had stderr piped");

    let status = process.wait()?;
    if !status.success() {
        let mut bytes = Vec::new();
        stderr.read_to_end(&mut bytes)?;
        let string_lossy = String::from_utf8_lossy(&bytes).into_owned();
        Err(Error::GraphVizFailed(string_lossy))?;
    }

    let mut buf = [0; 1024];
    loop {
        match stdout.read(&mut buf)? {
            0 => break,
            count => dest.write_all(&buf[.. count])?,
        }
    }

    Ok(())
}
