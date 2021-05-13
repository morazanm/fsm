// Order does not matter
const PUBLICATIONS = [
    {
        title: "Functional Automata - Formal Languages for Computer Science Students",
        authors: ["Marco T. Morazán", "Rosario Antunez"],
        description: "An introductory formal languages course exposes advanced undergraduate and early graduate \
                    students to automata theory, grammars, constructive proofs, computability, and decidability.\
                    Programming students...",
        link: "https://arxiv.org/abs/1412.4878",
    },
    {
        title: "Visual Designing and Debugging of Deterministic Finite-State Machines in FSM",
        authors: ["Marco T Morazán", "Joshua Schappel", "Sachin Mahashabde"],
        description: "This article presents a visualization tool for designing and debugging deterministic ﬁnite-state \
                        machines in FSM–a domain speciﬁc language for the automata theory classroom. Like other...",
        link: "https://arxiv.org/abs/2008.09254",
    },
    {
        title: "FSM Error Messages",
        authors: ["Marco T. Morazán", "Josephine A. Des Rosiers"],
        description: "Computer Science students, in general, find Automata Theory difficult and mostly unrelated to their area of study. To mitigate these perceptions, FSM...",
        link: "https://arxiv.org/abs/1906.11421v1",
    },
];


// Order from most recent to least recent
// Only the 5 most recent will be displayed
const VERSION_HISTORY = [
    {
        version: 1.6,
        notes: [
            "Bug fixes",
            "Viz tool quality of life improvements",
            "New FSM website",
        ]
    },
    {
        version: 1.5,
        notes: [
            "Fixed bug for show-transitions for Turing machines",
            "Fixed bug in rename-states",
            "fixed bug in consume in tm.rkt for Language Recognizers",
            "Fixed rename states for Turing machines and Language Recognizers",
        ]
    },
    {
        version: 1.4,
        notes: [
            "Added sm-graph",
            "Added colorblind mode to sm-graph",
            "Minor graphical changes to the visualization tool (new color 😊)",
        ]
    },
    {
        version: 1.3,
        notes: [
            "Added Turing Machine support to visualization tool",
            "Minor bug fixes with the visualization tool",
        ]
    },
    {
        version: 1.2,
        notes: [
            "Added Pushdown Automata support to the visualization tool",
            "Added scroll bars to input field for visualization tool",
        ]
    },
];
