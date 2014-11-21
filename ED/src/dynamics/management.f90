!==========================================================================================!
! Management.f90. These subroutines will control the management at patch level based on    !
!                 cut (harvesting), planting, herbicide. Low intensity fire is in ***.f90  !
! At first round, I will focus on cutting a patch                                          !
!                                                                                          !
! NOTICE:  These subroutines have not been thoroughly tested. Please                       !
!report problems to Mike Dietze dietze@bu.edu Afshin Pourmokhtarian, apourmok@bu.edu       !
!==========================================================================================!
! Check with Mike ==> Do we call this "management" subroutine from "Disturbance" similar to "Forestry" and let rest of calculations/updates happen in there (e.g. energy, run off, etc)?!
! I assume we won't use bigleaf scheme if we want to apply management, right?!
! Read management file and info for execution
subroutine read_management_xml(filename,success)


end subroutine read_management_xml


! This is the main part of the management file, I started for patch level, it will expand to higher level (e.g. site, polygon) later.
subroutine management_event(year)


! Define the variables, functions and other subroutines of ED that we need to execute this file. Again, the focus is on patch now!

 

   use ed_state_vars        , only : polygontype                & ! structure   !For later
                                   , sitetype                   & ! structure   !For later
                                   , patchtype                  & ! structure
                                   , allocate_sitetype          & ! subroutine   !For later
                                   , deallocate_sitetype        & ! subroutine   !For later
                                   , copy_sitetype_mask         ! ! subroutine
   use disturb_coms         , only : min_patch_area             & ! intent(in)
                                   , management_year            & ! intent(in)   ! Need to update this in ed_params.f90
                                   , harvest_age                ! ! intent(in)   ! It is set to 50, do we need to define it as input file or in ed_params.f90? 
   use disturbance_utils    , only : initialize_disturbed_patch & ! subroutine
                                   , plant_patch                ! ! subroutine   ! Check to see if we need to modify this for management.
   use fuse_fiss_utils      , only : terminate_patches          ! ! subroutine   ! Mike said first let's the model take care of fuse/fiss, if problem, we will address it.
   use ed_max_dims          , only : n_pft                      & ! intent(in)   ! Note; my branch of ED has 22 PFT which is still not on the main line.
                                   , n_dbh                      ! ! intent(in)
   use grid_coms            , only : nzg                        & ! intent(in)   !For later
                                   , nzs                        ! ! intent(in)   !For later
   use budget_utils         , only : update_budget              ! ! intent(in)
   implicit none

   !----- Arguments -----------------------------------------------------------------------!
   type(polygontype)             , target      :: cpoly   ! Not needed now.
   integer                       , intent(in)  :: year
   integer                       , intent(in)  :: isi
   !----- Local variables -----------------------------------------------------------------!
   type(sitetype)                , pointer     :: csite   ! Not needed now.
   type(patchtype)               , pointer     :: cpatch   
   type(sitetype)                , pointer     :: tempsite   ! Not needed now.
   logical        ,  dimension(:), allocatable :: mask
   integer                                     :: ipft
   integer                                     :: idbh
   integer                                     :: newp
   integer                                     :: iyear
   integer                                     :: useyear
   integer                                     :: ipa
   integer                                     :: ico
   real                                        :: first_harvest_target   !Fraction of patch to harvest?
   real                                        :: second_harvest_target   !I assume maximum number of harvest per each year under management is 2, or do they cut everything at once?
   real                                        :: total_site_biomass
   real                                        :: area_first_harvest   !What is the area within the patch?
   real                                        :: agb_first_harvest   !How much of AGB we will harvest?
   real                                        :: area_second_harvest
   real                                        :: agb_second_harvest
   real                                        :: lambda_first_harvest   !Harvest rate
   real                                        :: lambda_second_harvest
   real                                        :: harvest_target   ! Define how much management we will apply
   real                                        :: total_site_biomass   ! We need this to begin the managemenet so we can track what happens during and after management.
   real                                        :: agb_harvest   ! How much of AGB we will harvest?
   real                                        :: total_harvested_area

   !---------------------------------------------------------------------------------------!

 

   csite => cpoly%site(isi)





   ! CRAZY idea! Do we need to calculate targeted biomass when we apply herbicide? I am thinking herbicide code could be very similar to selective cut and could be implemented !
   ! within harvesting subroutine as well? Or does it make the code complicated?!

   !---------------------------------------------------------------------------------------!
   ! The patch is harvested up to a harvest_target.      !
   ! Should we add a flag to make sure the target biomass is not more than available biomass? Or we use fraction?! (I have the variable for fraction)
   ! ***I need to check if management works when Hurtt's module is active (later) !
   !---------------------------------------------------------------------------------------!



 
   !---------------------------------------------------------------------------------------!
   ! At this point I need to know PFTs that will be targeted and percentage based on DBH   !
   ! that selective logging must be applied.                                               !
   !---------------------------------------------------------------------------------------!

   harvest_target   = 

   !---------------------------------------------------------------------------------------!

   ! I am assuming that there are only 2 harvests per year, if that is not the case, modify this (check with Mike)!

   !----- The total target is the sum of both targets. ------------------------------------!
   total_harvest_target     = first_harvest_target + second_harvest_target
   !---------------------------------------------------------------------------------------!


 
   ! We want to track the biomass explicitly in the this module (Mike's rec.)!
   ! The AGB is sum of all PFTs and DBHs in the patch ==> at larger scale, we need to implement the same rational!

   !---------------------------------------------------------------------------------------!
   !    Finding total biomass density in kgC/m2.                                           !
   !---------------------------------------------------------------------------------------!
   total_site_biomass = 0.
   pftagbloop: do ipft=1,n_pft
      pftdbhloop: do idbh=1,n_dbh
         total_site_biomass = total_site_biomass + cpoly%agb(ipft, idbh, isi)
      end do pftdbhloop
   end do pftagbloop
   !---------------------------------------------------------------------------------------!


   
   !---------------------------------------------------------------------------------------!
   ! I am not sure if we need a flag below but in case the user mess up and prescribe 2 management and wipe out all vegetation! (check with Mike)!
   !     If site has no biomass, or if the area to be harvested is less than the minimum   !
   ! area for a new patch, do not harvest, and update the memory for the next year. I assumed there will be maximum of 2 harvests for each year.!
   !---------------------------------------------------------------------------------------!
   if (total_site_biomass == 0.0 .or.                                                      &
       total_harvest_target <= total_site_biomass * min_patch_area) then
      cpoly%first_harvest_memory(isi)   = first_harvest_target
      cpoly%second_harvest_memory(isi) = second_harvest_target
      return
   end if
   !---------------------------------------------------------------------------------------!

     !----- Compute the patch AGB --------------------------------------------------------!
      ! Do I need a flag in case the biomass is low (e.g. user mistake in harvesting rate or year?!

      !----- Skip the patch if the biomass is low. ----------------------------------------!
      if (csite%plant_ag_biomass(ipa) < 0.01) cycle patchloop


      !----- checks to see if harvest is possible------!
      first_harvest     = 
  
      second_harvest   = 

 
   !------ Compute current stocks of agb in targeted patches. -------------------------------!
   call inventory_harvest_forests(cpoly,isi,area_first_harvest,agb_first_harvest         &
                             ,area_second_harvest,agb_second_harvest)     

   !------ Compute the first round of harvest rates. (assuming maximum of 2 rounds)--------!
   call mat_forest_harv_rates(agb_first_harvest,agb_second_harvest                         &
                             ,first_harvest_target                                         &
                             ,second_harvest_target,lambda_first_harvest                   &
                             ,lambda_second_harvest)                                    

   !------ Apply logging to targeted stands. -----------------------------------------!
   call harv_mat_patches(cpoly,isi,newp,lambda_first_harvest                              &
                        ,lambda_second_harvest)
   
   !---------------------------------------------------------------------------------------!
   !     Compute harvested area from targeted  patches.                                    !
   !---------------------------------------------------------------------------------------!
   total_harvested_area = lambda_first_harvest    * area_first_harvest                   &
                        + lambda_second_harvest  * area_second_harvest  
                        
    !Do we need this flag? We might in case the user manegement event leaves us with very small patch and will cause later problem (check with Mike)!
   !---------------------------------------------------------------------------------------!
   !     Now we know the area of the new patch, and can normalize the averaged patch       !
   ! quantities. But this is done only when the new patch area is significant otherwise,   ! 
   ! just terminate it.                                                                    !
   !---------------------------------------------------------------------------------------!
 
      call norm_harv_patch(csite,newp)
      
      !----- Update temperature and density. ----------------------------------------------!
      call update_patch_thermo_props(csite,newp,newp,nzg,nzs,cpoly%ntext_soil(:,isi))



   !----- Eliminate those patches with small area. ----------------------------------------!
   call terminate_patches(csite)

   !------We need to write the output of harvest in terms of AGB and area (?) before we clear the momory to track the management through run---!

   ! call the output and write them out in annual (I am assuming we don't need monthly for management) (Check with Mike) !
   
   !----- Clear out the primary harvest memory. -------------------------------------------!
   cpoly%primary_harvest_memory(isi) = 0.0

   ! Tomorrow will be better when I cut down all these trees!!!!
   
   return
end subroutine management_event
!==========================================================================================!
!==========================================================================================!


!*** Check with Mike about first and second approach but probably in plantation, they cut their targeted area all at once ***!
  ! When we want to implement cross-patch harvest, do we provide input for each patch or at site level?!
   ! I am thinking if we can't harvest our target biomass from one patch, should we ask code to get the deficit from another one to reach total target?! Or maybe I am just overthinking it...!

! Do we need harvest_deficit? Probably not for plantation since they know their expected return for each harvest (check with Mike)!

!==========================================================================================!
!==========================================================================================!
subroutine harvest_rates(agb_first_harvest,agb_second_harvest,first_harvest_target,second_harvest_target,lambda_first_harvest            &
                                ,lambda_second_harvest,harvest_deficit)
   implicit none
   !----- Arguments -----------------------------------------------------------------------!
   real, intent(in)    :: agb_first_harvest
   real, intent(in)    :: agb_second_harvest
   real, intent(in)    :: first_harvest_target
   real, intent(inout) :: second_harvest_target
   real, intent(out)   :: lambda_first_harvest
   real, intent(out)   :: lambda_second_harvest
   real, intent(out)   :: harvest_deficit
   !---------------------------------------------------------------------------------------!


 
   ! We need to provide harvesting rate as input but what if not all targeted patch is mature?!
   ! In this case, don't we need to calculate a ratio of "available" biomass for harvest with a message for user?!
 
   if (agb_first_harvest > first_harvest_target) then
      lambda_first_harvest = first_harvest_target / agb_first_harvest
   else
      lambda_first_harvest    = 1.0
      harvest_deficit          = first_harvest_target   - agb_first_harvest
      second_harvest_target = second_harvest_target + harvest_deficit
   end if

 
   return
end subroutine harvest_rates
!==========================================================================================!
!==========================================================================================!


 !Time to cut these trees!
 
!==========================================================================================!
!==========================================================================================!
subroutine harvest_patches(cpoly,isi,newp,lambda_first_harvest,lambda_second_harvest)
   use ed_state_vars    , only : polygontype          & ! structure
                               , sitetype             & ! structure
                               , patchtype            ! ! structure

    use ed_max_dims      , only : n_pft                ! ! intent(in)
 

   implicit none
   !----- Arguments -----------------------------------------------------------------------!
   type(polygontype)                  , target     :: cpoly
   integer                            , intent(in) :: isi
   integer                            , intent(in) :: newp
   real                               , intent(in) :: lambda_first_harvest
   real                               , intent(in) :: lambda_second_harvest
   !----- Local variables -----------------------------------------------------------------!
   type(sitetype)                     , pointer    :: csite
   type(patchtype)                    , pointer    :: cpatch
   real             , dimension(n_pft)             :: mindbh_harvest
   integer                                         :: ipa
   integer                                         :: ico
   logical                                         :: first_harvest
   logical                                         :: second_harvest
   real                                            :: dA




   !----- Loop over patches. --------------------------------------------------------------!
   csite => cpoly%site(isi)

   do ipa=1,csite%npatches

      cpatch => csite%patch(ipa)

! again do we need a flag to check if we can harvest a patch or not? I am guessing in production forestry we do NOT! 
! I already have a flag earlier, if we want this flag here, pass the earlier info to here so we do not harvest them!
   
         !first harvest!
         dA                      = csite%patch(ipa) * lambda_first_harvest
         mindbh_harvest(1:n_pft) = csite%mindbh_first(1:n_pft,isi)
 
         !second harvest!
         dA                      = csite%patch(ipa) * lambda_second_harvest
         mindbh_harvest(1:n_pft) = csite%mindbh_second(1:n_pft,isi)
 
          do ico=1,cpatch%ncohorts
                 
                 pft = cpatch%pft(ico)    
             
    ! *** make sure to update the carbon pools of plants (f_labile) similar to Disturbance as Mike instructed==> we just target above ground, do not do anything with belowground***!

    !We assume we cut from the soil level, so whatever is left in ground will go for decomposition. I need to work on details of above ground (e.g. sapwood, leaf...)!
        
                 !********************Updating plant C! (Took this chunk from Events.f90, need further details and update! (Talk to Mike)**********************!

                 ialloc     =
                 bdead_new  = 
                 bswa_new   = 
                 bswb_new   = 

                 bstore_new = 
                 bleaf_new  = 
                 bfr_new    = 

                 cpatch%balive(ico)    = 
                 cpatch%broot(ico)     =
                 cpatch%bsapwooda(ico) = 
                 cpatch%bsapwoodb(ico) = 
                 cpatch%bdead(ico)     = 
                 cpatch%bstorage(ico)  =
                 
                                 
                 !----- Update LAI, WAI, and CAI ------------------------------------------!
                 call area_indices()

                 !----- Update basal area and above-ground biomass. -----------------------!
                 cpatch%basarea(ico) =           
                 cpatch%agb(ico)     =  

   return
end subroutine harvest_patches
!==========================================================================================!
!==========================================================================================!

! Here we need to normalize the patch characteristics after harvest!


!==========================================================================================!
!==========================================================================================!
subroutine normalize_harvest_patches(csite,newp)

   use ed_state_vars , only : sitetype            & ! structure
                            , patchtype           ! ! structure
   use disturb_coms  , only : min_patch_area      ! ! intent(in)
   use ed_max_dims   , only : n_pft               ! ! intent(in)
   use grid_coms     , only : nzg                 & ! intent(in)
                            , nzs                 ! ! intent(in)
   implicit none
   !----- Arguments -----------------------------------------------------------------------!
   type(sitetype), target     :: csite
   integer       , intent(in) :: newp
   !----- Local variables -----------------------------------------------------------------!
   real                       :: area_fac
   integer                    :: k
   !---------------------------------------------------------------------------------------!
   
   ! ***************This section is copied from forestry.f90******************!
   
   !Mike, Do we need "skip normalization when the patch is small?***!

   !----- Skip normalization when the patch is too small.  It will be terminated soon. ----!
   if (csite%area(newp) < min_patch_area) then
      return
   else
      !----- To make the values the weighted average of all contributing patches. ---------!
      area_fac = 1.0 / csite%area(newp)
   end if

   csite%fast_soil_C(newp)                 = csite%fast_soil_C(newp)         * area_fac
   csite%slow_soil_C(newp)                 = csite%slow_soil_C(newp)         * area_fac
   csite%structural_soil_C(newp)           = csite%structural_soil_C(newp)   * area_fac
   csite%structural_soil_L(newp)           = csite%structural_soil_L(newp)   * area_fac
   csite%mineralized_soil_N(newp)          = csite%mineralized_soil_N(newp)  * area_fac
   csite%fast_soil_N(newp)                 = csite%fast_soil_N(newp)         * area_fac
   csite%sum_dgd(newp)                     = csite%sum_dgd(newp)             * area_fac
   csite%sum_chd(newp)                     = csite%sum_chd(newp)             * area_fac
   csite%can_theta(newp)                   = csite%can_theta(newp)           * area_fac
   csite%can_theiv(newp)                   = csite%can_theiv(newp)           * area_fac
   csite%can_vpdef(newp)                   = csite%can_vpdef(newp)           * area_fac
   csite%can_prss(newp)                    = csite%can_prss(newp)            * area_fac
   csite%can_co2(newp)                     = csite%can_co2(newp)             * area_fac
   csite%can_shv(newp)                     = csite%can_shv(newp)             * area_fac
   csite%can_depth(newp)                   = csite%can_depth(newp)           * area_fac
   csite%ggbare(newp)                      = csite%ggbare(newp)              * area_fac
   csite%ggveg(newp)                       = csite%ggveg(newp)               * area_fac
   csite%rough(newp)                       = csite%rough(newp)               * area_fac
   csite%today_A_decomp(newp)              = csite%today_A_decomp(newp)      * area_fac
   csite%today_Af_decomp(newp)             = csite%today_Af_decomp(newp)     * area_fac
   csite%repro(1:n_pft,newp)               = csite%repro(1:n_pft,newp)       * area_fac
   csite%fsc_in(newp)                      = csite%fsc_in(newp)              * area_fac
   csite%ssc_in(newp)                      = csite%ssc_in(newp)              * area_fac
   csite%ssl_in(newp)                      = csite%ssl_in(newp)              * area_fac
   csite%fsn_in(newp)                      = csite%fsn_in(newp)              * area_fac
   csite%total_plant_nitrogen_uptake(newp) = csite%total_plant_nitrogen_uptake(newp)       &
                                           * area_fac

   do k = 1, nzs
      csite%sfcwater_mass(k,newp)   = csite%sfcwater_mass(k,newp)   * area_fac
      csite%sfcwater_energy(k,newp) = csite%sfcwater_energy(k,newp) * area_fac
      csite%sfcwater_depth(k,newp)  = csite%sfcwater_depth(k,newp)  * area_fac
   end do
   do k = 1, nzg
      csite%soil_energy(k,newp) = csite%soil_energy(k,newp) * area_fac
      csite%soil_water(k,newp)  = csite%soil_water(k,newp)  * area_fac
   end do
   return
end subroutine normalize_harvest_patches
!==========================================================================================!
!==========================================================================================!



























  
                 
                



