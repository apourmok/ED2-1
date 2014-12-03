!==========================================================================================!
! Management.f90. These subroutines will control the management at PATCH level based on    !
!                 cut (harvesting), planting, herbicide. Low intensity fire for later      !
! This subroutine will be called from "Disturbance", similar to "Forestry"                 !
! NOTICE:  These subroutines have not been thoroughly tested (AGU runs). Please            !
!report problems to Mike Dietze dietze@bu.edu Afshin Pourmokhtarian, apourmok@bu.edu       !
!==========================================================================================!
! I assume we won't use bigleaf scheme if we want to apply management.

! Read management file and info for execution
subroutine read_management_xml(filename,success)


end subroutine read_management_xml


! This is the main part of the management file for patch level, it will expand to upper levels (e.g. polygon, grid) later.
subroutine management_event_log(csite, isi, year)


! Define the variables, functions and other subroutines of ED that we need to execute this file. 

 

   use ed_state_vars        , only : sitetype                   & ! structure   
                                   , patchtype                  & ! structure
                                   , allocate_sitetype          & ! subroutine   
                                   , deallocate_sitetype        & ! subroutine   
                                   , copy_sitetype_mask         ! ! subroutine
   use disturb_coms         , only : ianth_disturb              & ! intent(in)   !If ianth_disturb == 0, need a flad to tell the user the anthropogenic disturbance is off! 
                                   , min_patch_area             & ! intent(in)
                                   , management_year            & ! intent(in)   ! Need to update this in ed_params.f90
                                   , harvest_age                ! ! intent(in)   ! *****It is set to 50, do we need to define it as input file or in ed_params.f90? 
   use disturbance_utils    , only : initialize_disturbed_patch & ! subroutine
                                   , plant_patch                ! ! subroutine   ! Check to see if we need to modify this for management.
   use fuse_fiss_utils      , only : terminate_patches          ! ! subroutine   ! Let's the model take care of fuse/fiss, if problem, we will address it.
   use ed_max_dims          , only : n_pft                      & ! intent(in)   ! Note; my branch of ED has 22 PFT which is still not on the main line.
                                   , n_dbh                      ! ! intent(in)
   use grid_coms            , only : nzg                        & ! intent(in)   
                                   , nzs                        ! ! intent(in)   !For later
   use budget_utils         , only : update_budget              ! ! intent(in)
   implicit none

   !----- Arguments -----------------------------------------------------------------------!
   integer                       , intent(in)  :: year
   integer                       , intent(in)  :: isi
   !----- Local variables -----------------------------------------------------------------!
   type(sitetype)                , pointer     :: csite   
   type(patchtype)               , pointer     :: cpatch   
   type(sitetype)                , pointer     :: tempsite   
   logical        ,  dimension(:), allocatable :: mask
   integer                                     :: ipft
   integer                                     :: idbh
   integer                                     :: newp
   integer                                     :: ipa
   integer                                     :: ico
   real                                        :: area_harvest   !What is the area within the patch?
   real                                        :: agb_harvest   !How much of AGB we will harvest?
   real                                        :: lambda_harvest   !Harvest fraction
   real                                        :: harvest_target   !This is in kgC/m2 (biomass)
   real                                        :: total_patch_biomass   ! We need this to begin the managemenet so we can track what happens during and after management.
   real                                        :: total_harvested_area   !Compute harvested area from targeted  patches

   !---------------------------------------------------------------------------------------!

   csite => cpoly%site(isi)

   !---------------------------------------------------------------------------------------!
   ! ***The patch is harvested up to a harvest_target (kgC/m2) but I think it is easier to only use "lambda_harvest" as input!
   ! ***If we want to use harvest_target, we need a flag to make sure the "harvest_target" is not more than "total_site_biomass"!
   ! ****I need to check if management works when Hurtt's module is active (Post-AGU) !
   !---------------------------------------------------------------------------------------!


   ! We want to track the biomass explicitly!
   ! The AGB is sum of all PFTs and DBHs in the patch ==> at larger scale, we need to implement the same rational (Some of patches and all the way up)!

   !---------------------------------------------------------------------------------------!
   !    Computing total patches biomass density of patch in kgC/m2.                                  !
   ! Added patchloop for future when we want to run for at site level with multiple patches!
   !---------------------------------------------------------------------------------------!
   total_patch_biomass = 0.
   csite => cpoly%site(isi)
   patchloop: do ipa=1,csite%npatches
      cpatch => csite%patch(ipa)
       pftagbloop: do ipft=1,n_pft
          pftdbhloop: do idbh=1,n_dbh
             total_patch_biomass = total_patch_biomass + csite%npatch%agb(ipft, idbh, isi)
          end do pftdbhloop
       end do pftagbloop
   end do patchloop
   ! ****Do we need a flag if targeted patch has no biomass or below some limit? (like below)****   !
   if total_patch_biomass == 0.0 ! Throw an error for the user
   !---------------------------------------------------------------------------------------!
  
 
   !------ Compute current stocks of agb in targeted patches. -------------------------------!
   csite => cpoly%site(isi)
   patchloop: do ipa=1,csite%npatches

      cpatch => csite%patch(ipa)

      !----- Compute the patch AGB --------------------------------------------------------!
      csite%plant_ag_biomass(ipa) = 0.0
      do ico=1,cpatch%ncohorts
         csite%plant_ag_biomass(ipa) = csite%plant_ag_biomass(ipa) + cpatch%agb(ico) * cpatch%nplant(ico)
      end do

      !----- Skip the patch if the biomass is low. ----------------------------------------!
      if (csite%plant_ag_biomass(ipa) < 0.01) cycle patchloop
        
                                  

   !------ Apply logging to targeted stands. -----------------------------------------!
   call harvest_patches(csite,isi,newp,lambda_harvest)                              
   
   


                     
   
   !---------------------------------------------------------------------------------------!
   !     Compute harvested area from targeted patches.                                    !
   !---------------------------------------------------------------------------------------!
   total_harvested_area = lambda_harvest * area_harvest                   
                          
                        
   
   !---------------------------------------------------------------------------------------!
   !     Now we know the area of the new patch, and can normalize the averaged patch       !
   ! quantities. But this is done only when the new patch area is significant otherwise,   ! 
   ! just terminate it.                                                                    !
   !---------------------------------------------------------------------------------------!
 
   call norm_harv_patch(cpatch,newp)
      
   !----- Update temperature and density. ----------------------------------------------!
   call update_patch_thermo_props(cpatch,newp,newp,nzg,nzs,cpatch%ntext_soil(:,isi))



   !----- Eliminate those patches with small area. ----------------------------------------!
   call terminate_patches(cpatch)

   !------We need to write the output of harvest in terms of AGB and area (?) before we clear the momory to track the management through run---!

   ! call the output and write them out in annual (Do we need monthly output for management) (Check with Mike) !
   
   !----- Clear out the primary harvest memory. -------------------------------------------!
   csite%harvest_memory(isi) = 0.0

 
   
   return
end subroutine management_event_log
!==========================================================================================!
!==========================================================================================!



 
!==========================================================================================!
!==========================================================================================!
subroutine harvest_patches(cpatch,isi,newp,lambda_harvest)
   use ed_state_vars    , only : sitetype             & ! structure
                               , patchtype            ! ! structure

    use ed_max_dims      , only : n_pft                ! ! intent(in)
 

   implicit none
   !----- Arguments -----------------------------------------------------------------------!
   integer                            , intent(in) :: isi
   integer                            , intent(in) :: newp
   real                               , intent(in) :: lambda_harvest
   
   !----- Local variables -----------------------------------------------------------------!
   type(sitetype)                     , pointer    :: csite
   type(patchtype)                    , pointer    :: cpatch
   real             , dimension(n_pft)             :: mindbh_harvest
   integer                                         :: ipa
   integer                                         :: ico
   logical                                         :: harvest
   real                                            :: dA



   !----- Loop over patches. --------------------------------------------------------------!
   
   patchloop:  do ipa=1,csite%npatches
      
      !----- Skip the patch with too little biomass ---------------------------------------!
      if (csite%plant_ag_biomass(ipa) < 0.01) cycle patchloop


      cpatch => csite%patch(ipa)
   
         !harvest area!
         dA                      = csite%patch(ipa) * lambda_harvest
         mindbh_harvest(1:n_pft) = csite%mindbh(1:n_pft,isi)
 
         cpatch=>csite%patch(ipa)
         do ico=1,cpatch%ncohorts
                 
                 pft = cpatch%pft(ico)    
             
    ! Update the carbon pools of plants (f_labile) similar to Disturbance as Mike instructed==> we just target above ground, do not do anything with belowground!

    !We assume we cut from the soil level, so whatever is left in ground will go for decomposition. I need to work on details of above ground (e.g. sapwood, leaf...)!
        
                 !Updating plant C!

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
! Call the function from "Forestry.f90"!


call norm_harv_patch(csite,newp)   !****should I move this to inside of harvest_patches loop?!



!=======================================================================================!
!    Pesticide/Herbicide application at patch level                                     !
!---------------------------------------------------------------------------------------!
subroutine plant_pest_herb()




















   !The subroutine below is for planting after logging and/or applying herbicide/pesticide.

   !=======================================================================================!
   !=======================================================================================!
   !    Add cohorts of the prescribed PFTs type to logged patch                            !
   !---------------------------------------------------------------------------------------!
   subroutine plant_patch(csite,np,mzg,pft,density,ntext_soil,green_leaf_factor            &
                         ,height_factor,lsl)
      use ed_state_vars , only  : sitetype                 & ! structure
                                , patchtype                ! ! structure
      use pft_coms       , only : q                        & ! intent(in)
                                , qsw                      & ! intent(in)
                                , sla                      & ! intent(in)
                                , hgt_min                  & ! intent(in)
                                , hgt_max                  ! ! intent(in)                                
      use ed_misc_coms   , only : dtlsm                    ! ! intent(in)                                
      use fuse_fiss_utils, only : sort_cohorts             ! ! sub-routine
      use ed_therm_lib   , only : calc_veg_hcap            ! ! function
      use consts_coms    , only : t3ple                    & ! intent(in)
                                , pio4                     ! ! intent(in)
      use allometry      , only : h2dbh                    & ! function
                                , dbh2bd                   & ! function
                                , area_indices             & ! function
                                , ed_biomass               ! ! function
      use ed_max_dims    , only : n_pft                    ! ! intent(in)   ! I have 22 PFTs in my branch.
      use phenology_coms , only : retained_carbon_fraction ! ! intent(in)
      use phenology_aux  , only : pheninit_balive_bstorage ! ! intent(in)
      use therm_lib      , only : cmtl2uext                ! ! intent(in)
      implicit none
      !----- Arguments. -------------------------------------------------------------------!
      type(sitetype)                  , target     :: csite
      integer                         , intent(in) :: mzg
      integer                         , intent(in) :: np
      integer                         , intent(in) :: pft
      integer                         , intent(in) :: lsl
      integer       , dimension(mzg)  , intent(in) :: ntext_soil
      real          , dimension(n_pft), intent(in) :: green_leaf_factor
      real                            , intent(in) :: density
      real                            , intent(in) :: height_factor
      !----- Local variables. -------------------------------------------------------------!
      type(patchtype)                 , pointer    :: cpatch
      type(patchtype)                 , pointer    :: tpatch
      integer                                      :: nc
      real                                         :: salloc
      real                                         :: salloci
      real                                         :: bleaf_max
      real                                         :: balive_max
      !------------------------------------------------------------------------------------!


      cpatch => csite%patch(np)

      
      !------------------------------------------------------------------------------------!
      ! Assuming that we apply planting to the same patches that we just harvested         !
      !------------------------------------------------------------------------------------!
      nc = cpatch%ncohorts + 1
 
      cpatch%ncohorts = nc
      cpatch%pft(nc)    = pft
      cpatch%nplant(nc) = density
 
         !---------------------------------------------------------------------------------!
         !    SAS approximation, assign height and use it to find DBH and the structural   !
         ! (dead) biomass. (Similar to Forestry.f90)                                       !
         !---------------------------------------------------------------------------------!
         cpatch%hite (nc) = hgt_min(cpatch%pft(nc)) * min(1.0,height_factor)
         cpatch%dbh  (nc) = h2dbh(cpatch%hite(nc),cpatch%pft(nc))
         cpatch%bdead(nc) = dbh2bd(cpatch%dbh(nc),cpatch%pft(nc))
         !---------------------------------------------------------------------------------!
      
      ! Rest if from Forestry.f90, check with Mike what to modify/remove!
   
      !----- Initialise other cohort-level variables. -------------------------------------!
      call init_ed_cohort_vars(cpatch, nc, lsl)
      !------------------------------------------------------------------------------------!

      !------------------------------------------------------------------------------------!
      !      Initialise the active and storage biomass scaled by the leaf drought          !
      ! phenology (or start with 1.0 if the plant doesn't shed their leaves due to water   !
      ! stress.                                                                            !
      !------------------------------------------------------------------------------------!
      call pheninit_balive_bstorage(mzg,cpatch%pft(nc),cpatch%krdepth(nc),cpatch%hite(nc)  &
                                   ,cpatch%dbh(nc),csite%soil_water(:,np),ntext_soil       &
                                   ,green_leaf_factor,cpatch%paw_avg(nc),cpatch%elongf(nc) &
                                   ,cpatch%phenology_status(nc),cpatch%bleaf(nc)           &
                                   ,cpatch%broot(nc),cpatch%bsapwooda(nc)                  &
                                   ,cpatch%bsapwoodb(nc),cpatch%balive(nc)                 &
                                   ,cpatch%bstorage(nc))
      !------------------------------------------------------------------------------------!



      !----- Compute all area indices needed. ---------------------------------------------!
      call area_indices(cpatch%nplant(nc),cpatch%bleaf(nc),cpatch%bdead(nc)                &
                       ,cpatch%balive(nc),cpatch%dbh(nc),cpatch%hite(nc),cpatch%pft(nc)    &
                       ,cpatch%sla(nc),cpatch%lai(nc),cpatch%wai(nc),cpatch%crown_area(nc) &
                       ,cpatch%bsapwooda(nc))


      !----- Find the new basal area and above-ground biomass. ----------------------------!
      cpatch%basarea(nc)= pio4 * cpatch%dbh(nc) * cpatch%dbh(nc)
      cpatch%agb(nc)    = ed_biomass(cpatch%bdead(nc),cpatch%bleaf(nc)                     &
                                    ,cpatch%bsapwooda(nc),cpatch%pft(nc))

      cpatch%leaf_temp    (nc) = csite%can_temp  (np)
      cpatch%leaf_temp_pv (nc) = csite%can_temp  (np)
      cpatch%leaf_water   (nc) = 0.0
      cpatch%leaf_vpdef   (nc) = csite%can_vpdef (np)
      cpatch%leaf_fliq    (nc) = 0.0
      cpatch%wood_temp    (nc) = csite%can_temp  (np)
      cpatch%wood_temp_pv (nc) = csite%can_temp  (np)
      cpatch%wood_water   (nc) = 0.0
      cpatch%wood_fliq    (nc) = 0.0

      !----- Because we assigned no water, the internal energy is simply hcap*T. ----------!
      call calc_veg_hcap(cpatch%bleaf(nc),cpatch%bdead(nc),cpatch%bsapwooda(nc)            &
                        ,cpatch%nplant(nc),cpatch%pft(nc)                                  &
                        ,cpatch%leaf_hcap(nc),cpatch%wood_hcap(nc))

      cpatch%leaf_energy(nc) = cmtl2uext(cpatch%leaf_hcap (nc),cpatch%leaf_water(nc)       &
                                        ,cpatch%leaf_temp (nc),cpatch%leaf_fliq (nc))
      cpatch%wood_energy(nc) = cmtl2uext(cpatch%wood_hcap (nc),cpatch%wood_water(nc)       &
                                        ,cpatch%wood_temp (nc),cpatch%wood_fliq (nc))
      call is_resolvable(csite,np,nc)

      !----- Sort the cohorts so that the new cohort is at the correct height bin. --------!
      call sort_cohorts(cpatch)

      return
   end subroutine plant_patch
   !=======================================================================================!







  
                 
                



