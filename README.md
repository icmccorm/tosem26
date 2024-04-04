# A Mixed-Methods Study on the Implications of Unsafe Rust for Interoperation, Encapsulation, and Tool Design
This is the replication package for our mixed-methods study on how Rust developers use `unsafe` code. All direct (e.g. names, institutions) and indirect (positions, projects) identifying information from participants has been redacted following procedures approved by our institution's IRB. 
The [Appendix](https://github.com/icmccorm/unsafe-study/blob/main/appendix.pdf) contains a complete copy of each survey, the interview protocol, consent scripts, and the materials we used for calculating interrater reliability when refining our codebook.

This dataset contains the following
```
- data               
    |- interviews           // raw interview transcripts
    |- community_survey     // survey responses
    |- screening_survey     // survey responses
    |- irr                  // codes and coding decisions 
- scripts                   // R and Python scripts that compile survey data       
- appendix.pdf         
```

## Building
Executing `make build` will compile our raw data into the tables and figures present in our paper and its appendix. This script requires R version 4.3.1 and Python 3. Alternatively, if you are running Docker on an x86 system, you can create an image with a build of our project by executing `docker build .` from the root directory. This image includes all necessary dependencies and builds the project automatically as its final step.

## Data
Raw data is located within the `data` directory. This contains all anonymized interview transcripts, responses to our screening and community surveys, and IRR scores.

### Interviews
```
- data
    |- interviews
        |- coding               // codebooks exported from ATLAS.ti
            |- codebook.csv     // individual codes and their definitions
            |- decisions.csv    // coding decisions
        |- transcripts          // markdown and .PDF transcripts exported from ATLAS.ti
```
We provide a full export of our ATLAS.ti project as well as unencoded copies of the raw interview transcripts and codebooks. 
Each participant is identified by a unique ID ranging from 1 to 19. 

### Surveys
```
- data
    |- community_survey
        |- coding
            |- unsafe_api.csv   // coded open-ended responses for WUQ1
            |- unsafe.csv       // coded open-ended responses for EUAQ1
        |- data.csv             // individual survey responses
        |- questions.csv        // mapping from question IDs to question text
        |- sections             // mapping from section IDs to section name
    |- screening_survey
        |- data.csv             // individual survey responses
        |- questions.csv        // mapping from question IDs to question text.
```
Each question in the screening and community surveys is identified by a unique ID. Questions in the community survey were grouped into sections. Each question ID contains a prefix identifying its section. For example, "WUQ1" is the 1st question in section 
"WUQ", which contains questions about participants' motivations for using unsafe code. 

### IRR
```
- data
    |- irr
        |- 1
           |- data.csv      // codes selected for each quote by each author
           |- output.md     // individual quotes coded in this sample
           |- survey.csv    // codes presented for each user to select
        |- 2
        |- ...
        |- code_mapping.csv
        |- theme_mapping.csv
```
We used interrater reliability as a mechanism for refining our codebook. We recorded coding decisions using an online form. The responses for each round are provided in the [irr](https://github.com/icmccorm/unsafe-study/tree/main/data/irr) directory within subdirectories numbered 1 through 7. Each subdirectory contains 3 files. The file [survey.csv](https://github.com/icmccorm/unsafe-study/blob/main/data/irr/1/survey.csv) contains the codes presented by the form for each coder to select during that round, while [data.csv](https://github.com/icmccorm/unsafe-study/blob/main/data/irr/1/data.csv) contains the selections made by each coder for each quote in [output.md](https://github.com/icmccorm/unsafe-study/blob/main/data/irr/1/output.md). In Section 5 of the Appendix, we describe this process in further detail and provide copies of the forms used for each round. 
