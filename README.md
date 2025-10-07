# Age differences as exposures in epidemiologic research

Contact: Christian Dudel, dudel@demogr.mpg.de 

The age difference between two persons is often of interest as an exposure in 
epidemiologic research. The age difference between mother and father and how it 
affects child health outcomes is an example. The code in this repository
shows that there are three key issues when using age differences as an exposure.

The file 'visual_representation.R' provides figures which demonstrate the
identification problem. The file 'birth_example.R' uses U.S. birth register data 
on more than 3 million births and models the influence of the maternal age, the 
paternal age, and the parental age difference on the risk of low birth weight; 
this example shows how to properly model the interaction between age variables.
The file 'simulations.R' uses simulations and artificial data to show that
re-parametrizing models including age and age differences can lead to quite
different results with respect to statistical inference. 
