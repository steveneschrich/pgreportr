#' The native order of investigator roles within the system.
#'
#' @details
#' Investigator roles follow a hierarchy from PI through consultant and mentor. This variable consists
#' of all known roles in order so that investigators can be arranged using this hierarchy.
#'
#' @export
role_order<-c("Contact PI","PI","Co-PI","MPI","PD/PI","Site PI","Core Co-PI",
              "co-I","Co-Leader","Bioinformatician","Biostatistician","Collaborator","CHE",
              "PostDoctotal","PhD Student","Community Leader", "consultant","Consultant",
              "Mentor", "Sponsor")

u54_cores <- c("QSC","PRBB","REC","Outreach Core","Admin Core","PEC")
u54_othersupport <- c("Research Project","US-LACRN Supplement","ESI (non-Partnership related)")
u54_support <- c(u54_cores, u54_othersupport)
rtype_grants <- c("R00","R01", "R03", "R15", "R21", "R25", "R50","R34")
