module parameters
    
	integer, parameter :: Side=30				!side of the paralleogram

	integer, parameter :: NoOfAtoms=180 !No of atoms to be added to simulation
	integer, parameter :: SizeAsIsland=10
	integer, parameter :: TimeInterval=180	!number of interval for each txt output
	real, parameter :: MaxTime=181, AtomsAddedPerCycle =0.09, TempIncPerCycle =0.5  
    !how many time will the time pass

	real(kind=8), parameter::Ed=0.550			!diffusion energy
	real(kind=8), parameter::Eb=0.055			!bonding energy
	real(kind=8), parameter::Tc=300   			!temp.

    real(kind=8), parameter::fc=2E10			!constant frequency
	real(kind=8), parameter::kB=8.625E-5		!Boltzmann constant in eV unit
    
    !Default settings:
    logical :: AtomsAddedOverTime, TempIncreaseOverTime, OutputWhenAtomsMoved
    

    
    end module