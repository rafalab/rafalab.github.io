---
layout: template1
title: COVID Zoomposium
---

![](covid/flyer.png)

## COVID-19 Data Science Symposium

After having to postpone our 2020 Zelen Symposium to September 30 co-organizer [Mark Hansen](https://twitter.com/cocteau) suggested we organize and host an online COVID-19 symposium instead. The event was held on April 2, 2020. Here we provide a summary and materials shared by the speakers.

Four speakers presented:

* Caroline Buckee [@CarolineOB](https://twitter.com/Caroline_OF_B), Department of Epidemiology, Harvard University, [Slides](covid/buckee.pdf)

* Mike Mina [@michaelmina_lab](https://twitter.com/michaelmina_lab), Department of Epidemiology, Harvard University, [Slides](covid/mina.pdf)
* Natalie Dean [@nataliexdean
](https://twitter.com/nataliexdean), Department of Biostatistics, University of Florida, [Slides](covid/dean.pdf)
* Alexis Madrigal [@alexismadrigal](https://twitter.com/alexismadrigal), The Atlantic, [Web resource](https://covidtracking.com/)

Hosts: 

* [Department of Data Science](http://datasciences.dfci.harvard.edu/) at Dana-Farber Cancer Institute
* [Brown Institute](https://brown.columbia.edu/) at Columbia Journalism School

Below are my takeaways from each talk (under construction) and the materials that were shared.

### Epidemiological modeling 

Presenter: Caroline Buckee

Slides: [PDF](covid/buckee.pdf)

My takeaways:
* Mathematical models provide a framework to simulate scenarios under different conditions. 
* These simulations are controlled by interpret able model 
parameters.
* One of these, $k$, represents the contact rate. Social distancing reduces $k$.
* For the model parameters you can derive R0, the average number of people that one person infects with a virus. 
* We can use these simulations to help guide policy decisions.
* You will see very different predictions reported by the media and wonder why do different experts give such different predictions. One reason is that the press may be selecting only one of the scenarios described by a scientific report. It's better to read the original source.
* As data becomes available we can try to estimate the parameters, but differences in testing and reporting make it difficult.
* For similar reasons fatality rates are hard to estimate. 
* Due to these challenges these models are not good for forecasting.
* Thanks to mobile devices and social network companies we now have access to mobility data that can be used to study the effects of social distancing.
* COVID-19 is clearly worse than the flu. It has a higher infectious rate, we have less immunity to it, and current fatality rate estimates are higher.


### Testing 

Presenter: Michael Mina

Slides: [PDF](covid/mina.pdf)

My takeaways:
* The general way the current tests work is by identifying unique parts of the virus' RNA and developing  [primers](https://www.nature.com/scitable/definition/primer-305/) that can be used to detect the presence of the virus in a human sample obtained from a nasal swab using [qPCR](https://en.wikipedia.org/wiki/Real-time_polymerase_chain_reaction).
* This is a straight-forward test that most hospitals and clinical laboratories are equipped to perform as long as the primers are made available. 
* One reason the testing started late in the US is because the CDC imposed a rule stating that all tests needed to be run by CDC. This was done due to quality control concenrs.
* A second reson is that the main companies that develop tests like these ()

* The WHO developed a test early on that worked well.

###  Vaccines & therapeutics

Presenter: Natalie Dean

Slides: [PDF](covid/dean.pdf)

My takeaways:

### Obtaining and Organizing Public Data


Presenter: Alexis Madrigal

Website: [The COVID Tracking Project](https://covidtracking.com/)

My takeaways:
