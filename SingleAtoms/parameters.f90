module parameters
    
	integer, parameter :: Side=5				!side of the paralleogram

	integer, parameter :: NoOfAtoms=2           !No of atoms to be added to simulation
	integer, parameter :: SizeAsIsland=2
	integer, parameter :: TimeInterval=1080, MaxTime=1E7,CycleRecord=1000	!number of interval for each txt output
	real, parameter :: AtomsAddedPerCycle =0.07, TempIncPerCycle =0.5  
    !how many time will the time pass

	real(kind=8), parameter::Ed=0.500			!diffusion energy
	real(kind=8), parameter::Eb=0.055			!bonding energy
	real(kind=8), parameter::Tc=300   			!temp.

    real(kind=8), parameter::fc=1E5		!constant frequency
	real(kind=8), parameter::kB=8.625E-5		!Boltzmann constant in eV unit
    
    !Default settings:
    logical :: AtomsAddedOverTime, TempIncreaseOverTime, OutputWhenAtomsMoved
    

    
    end module