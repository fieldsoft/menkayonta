# Menkayonta

It means \"workbook\" in Wao Terero, a linguistic isolate spoken in the
Ecuadorian Amazon.

The name is significant for a number of reasons.

1.  The lead developer is a linguistic field researcher, and the name
    celebrates the significance of the Wao people in his research.
2.  The project emphasizes notes and collaboration to a greater extent
    than past fieldwork software.
3.  The project emphasizes community engagement by considering the needs
    of language learners, as well as scientific research.

## Building

Two commands are needed once nix dependencies are set up.

npm install

npm run rebuild

## Audience

The emphasis is on the needs of the following groups:

-   Linguistic field researchers
-   Students in field research courses
-   Instructors of field research courses
-   Relatively untrained heritage speakers of a low resource language
-   Language learners of any language

There is also an emphasis on providing a tool with the flexibility to
accommodate research on the following:

-   Innovative user interfaces for language documentation and learning
-   The incorporation of AI to aid in research workflows and analysis
-   Computational experiments with small linguistic datasets

## Aims for the software

1.  Reproduce and improve the important functionality of
    [dative](https://github.com/dativebase), a precursor system to
    menkayonta.
    -   Provide a data entry and search interface for glossed linguistic
        data from elicitation.
    -   Allow data that is entered in the database to be shared with
        other researchers.
    -   Provide suggested parses and glosses to speed the process of
        morphological analysis.
    -   Allow for data to be imported and exported in a variety of
        common formats.
2.  Provide a collaborative notes and agenda environment.
    -   A rich markup language.
    -   TODO items.
    -   Agenda view to plan work or study.
    -   The ability to export reports, portions of technical papers,
        lesson plans, etc.
    -   (Planned) Programming notebook-like functionality to allow
        advanced users to run sandboxed scripts in python or other
        languages via webassembly over database data to produce
        visualization, analyses, etc.
3.  Offline first functionality
    -   No central server is needed for a private database
    -   Users may synchronize local databases with a shared central
        database
    -   (Planned) Peer-to-peer synchronization over WebRTC
4.  The ability to work with texts.
    -   Glossed line items may be viewed as texts.
5.  Lexicographic tools.
    -   Headwords (lexemes) may be defined with corresponding
        documentation of inflected forms, definitions, and links to
        sentences where they appear.
    -   Keywords for reverse lookup may be defined.
6.  AI enhanced analysis workflow
    -   At the minimum, suggested parses and glosses.
7.  The ability to define and run elicitation protocols
    -   A separate app can run the protocols and synchronize responses
        with the user\'s menkayonta instance.
8.  Intuitive, yet powerful UI
    -   Present the minimum complexity to the user by default
    -   Make the simple glossing tasks trivial
    -   Allow for efficient data entry

## Technologies and justifications

### [Electronjs](https://www.electronjs.org/)

In order to allow for offline first functionality, a desktop application
is needed. Web programming is the dominant paradigm for creating
end-user applications.

1.  Advantages of Electronjs over alternatives

    -   Utilizes technologies familiar to all web programmers
    -   Mature software stack used by some of the most popular
        cross-platform applications, such as vscode
    -   A single browser environment for all instances, avoiding webview
        incompatibilities
    -   Though the backend is not designed with Android or iOS in mind,
        the frontend is sufficiently abstracted that code reuse for
        mobile applications is possible
    -   Relatively stable API
    -   Excellent documentation

### [PouchDB](https://pouchdb.com/)

In order to provide robust database synchronization, a database with a
mature bi-directional synchronization mechanism is needed.

1.  Advantages of PouchDB over alternatives

    -   Mature
        -   More than a decade of development
        -   An incubating project of the [Apache Software
            Foundation](https://apache.org/)
        -   A stable API
        -   Compatible with a variety of technologies and products
            -   [Apache CouchDB](https://couchdb.apache.org/)
            -   [IBM Cloudant](https://www.ibm.com/products/cloudant)
            -   [Couchbase](https://www.couchbase.com/)
            -   [RxDB](https://rxdb.info/)
    -   Fully open source
    -   Quote from Jan Lehnardt, lead developer of CouchDB: \"PouchDB,
        while not immensely popular, is being used in a wide variety of
        FOSS projects and proprietary products, including a number of
        mission-critical, humanitarian and medical environments where no
        other software solution would suffice. PouchDB is important.\"
    -   Setting up database synchronization is trivial
    -   Conflict resolution is predictable
    -   JavaScript based, runs in the browser and in node
    -   Well documented
    -   CouchDB was used in the precursor to dative

### [Elm](https://elm-lang.org/)

The lead developer is a linguist. He doesn\'t have time to mess around
with high maintenance UI libraries, but needs complex UI code. A system
is needed that is easy to maintain, bug free, and safe to refactor, if
needed.

1.  Advantages of Elm over alternatives

    -   A pure typed functional programming language
        -   No mutable data structures
        -   Clear separation from effectful code
    -   Fast
    -   Designed for web UI and nothing else
    -   Designed to be easy for beginners
    -   Well documented
    -   Simple
    -   Easy to understand error messages
    -   Stable, effectively frozen
        -   Libraries written 10 years ago run without alteration
