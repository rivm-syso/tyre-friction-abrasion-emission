#'@param v_start Velocity at the start of the section of the track (m/s)
#'@param v_end Acceleration time (s)
#'@param dv Acceleration/Descelaration constant of the vehicle (m/s^2)
dv_distance <- function(v_start, v_end, dv) {
  dv_time <- abs(v_start-v_end)/dv
  v_start*dv_time+1/2*dv*dv_time^2
}
