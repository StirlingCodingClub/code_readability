# The CSV files are all in the 'CSV_files' directory on my local machine
# CSV_files dir has folders for 'Kenya', 'Orkney', 'Gabon' and 'Madagascar'

# Need to read all of the functions below to be able to clean the data
kenya  <- process_csv_files(dir = "CSV_files/Kenya", game_name = "Kenya");
orkney <- process_csv_files(dir = "CSV_files/Orkney", game_name = "Orkney");
gabon  <- process_csv_files(dir = "CSV_files/Gabon", game_name = "Gabon");
mada   <- process_csv_files(dir = "CSV_files/Madagascar", game_name = "Mada");

# Now just need to make the data table with the processed files
dat_tabl <- make_data_table(processed_files = c(kenya, orkney, gabon, mada),
                            save_file = "data.csv");

# Make sure to run the functions below into the console before doing the above
################################################################################
################################################################################
################################################################################
# Functions needed to clean and use the CSV files are below.
################################################################################
################################################################################
################################################################################

#' Make a data table from a list of processed CSV game files
#' 
#' Outer function for re-organising the data processed from the CSV files using
#' the `process_csv_files` function below and turning it into one big table that
#' is saved to a new CSV file.
#'
#'@return A table of game data extracted from CSV files
#'@param processed_files CSV files run through the `process_csv_files` function
#'@param save_file Path for the CSV file to which the output will be saved
#'@export
make_data_table <- function(processed_files, save_file = "data.csv"){
    temp_table <- list();
    for(i in 1:length(processed_files)){
        temp_table[[i]] <- do.call("rbind", processed_files[i]);
    }
    full_table <- do.call("rbind", temp_table);
    rownames(full_table) <- NULL;
    full_table           <- add_decision_time(full_table);
    write.csv(x = full_table, file = save_file, row.names = FALSE);
    return(full_table);
}


add_decision_time <- function(dat){
    s_h <- dat$start_hr;
    s_m <- dat$start_min;
    s_s <- dat$start_sec;
    c_h <- dat$confirm_hr;
    c_m <- dat$confirm_min;
    c_s <- dat$confirm_sec; 
    
    s_h[s_h < 6] <- s_h[s_h < 6] + 12;
    c_h[c_h < 6] <- c_h[c_h < 6 ] +12;
    st <- (s_h * 3600) + (s_m * 60) + s_s;
    en <- (c_h * 3600) + (c_m * 60) + c_s;
    decision_time_sec <- round(en - st, digits = 2);
    dat               <- cbind(dat, decision_time_sec);
    return(dat);
}

process_csv_files <- function(dir = getwd(), game_name){
    all_csvs <- list.files(path = dir, pattern = ".csv");
    num_csvs <- length(all_csvs);
    games    <- NULL;
    results  <- NULL;
    list_el  <- 1;
    for(i in 1:num_csvs){
        filename <- all_csvs[i];
        if( identical(dir, getwd()) == FALSE ){
            filename <- paste(dir,"/", all_csvs[i], sep= "");
        }
        check_file <- scan(file = filename, what = "character");
        if(check_file[1] == "Player" & check_file[2] == "1"){
            game_res            <- summarise_game_file(filename, game_name);
            results[[list_el]]  <- game_res;
            list_el             <- list_el + 1;
        }
    }
    return(results);
}

summarise_game_file <- function(filename, game_name = "default"){
    dat         <- scan(file = filename, what = "character");
    dat         <- clean_five_commas(dat); # Some files have five commas?
    tags        <- which(dat == "Tag:");
    game_type   <- dat[tags + 1];
    rounds      <- get_round_number(dat);
    Results     <- NULL;
    for(i in 1:rounds){
        start_time     <- get_round_start(dat, i);
        starts_time    <- rbind(start_time, start_time, start_time, start_time);
        confirm_time   <- get_player_confirm(dat, i);
        p_score        <- get_player_total_score(dat, i);
        rp             <- rep(x = i, times = 4);
        round_data     <- cbind(1:4, rp, starts_time, confirm_time, p_score);
        Results        <- rbind(Results, round_data);
    }
    rownames(Results)  <- NULL;
    colnames(Results)  <- NULL;
    namepos            <- which(dat == "HHID:") + 1;
    player_names       <- dat[namepos];
    Res_array          <- as.data.frame(Results);
    
    for(i in 1:dim(Res_array)[1]){
        player_number   <- as.numeric(Res_array[i, 1]);
        Res_array[i, 1] <- player_names[player_number];
    }
    
    cols <- c("HHID", "round", "start_hr", "start_min", "start_sec", 
              "confirm_hr", "confirm_min", "confirm_sec", "total_score");
    colnames(Res_array) <- cols;
    game_tag  <- rep(x = game_type, times = dim(Res_array)[1]);
    game_name <- rep(x = game_name, times = dim(Res_array)[1]);
    Res_array <- cbind(game_name, game_tag, Res_array);

    return(Res_array);
}


add_player_ID <- function(Res_array, player_names){
    for(i in 1:dim(Res_array)[1]){
        if(Res_array[i, 1] == 1){
            Res_array[i, 1] <- player_names[1];
        }
    }
}

get_round_number <- function(dat){
    rnd_pos <- which(dat == "Rounds:") + 1;
    if(dat[rnd_pos - 2] != "of" | dat[rnd_pos - 3] != "Number"){
        stop("Error in finding the round number");
    }
    rounds <- as.numeric(dat[rnd_pos]);
    return(rounds);
}

get_round_start <- function(dat, round){
    time_pos <- which(dat == "Time:")[round] + 1;
    if(dat[time_pos - 2] != "Start"){
        stop("Error in finding a round start time");
    }
    hms_start  <- hms_sep(dat[time_pos]);
    return(hms_start);
}

get_player_confirm <- function(dat, round){
    p1_pos <- which(dat == "Confirm")[(4 * (round - 1)) + 1] + 4;
    p2_pos <- which(dat == "Confirm")[(4 * (round - 1)) + 2] + 4;
    p3_pos <- which(dat == "Confirm")[(4 * (round - 1)) + 3] + 4;
    p4_pos <- which(dat == "Confirm")[(4 * (round - 1)) + 4] + 4;
    if(as.numeric(strsplit(dat[p1_pos - 1], split = ":")[[1]][1]) != round){
        stop("Round number error in confirm time player 1")
    }
    if(as.numeric(strsplit(dat[p2_pos - 1], split = ":")[[1]][1]) != round){
        stop("Round number error in confirm time player 2")
    }
    if(as.numeric(strsplit(dat[p3_pos - 1], split = ":")[[1]][1]) != round){
        stop("Round number error in confirm time player 3")
    }
    if(as.numeric(strsplit(dat[p4_pos - 1], split = ":")[[1]][1]) != round){
        stop("Round number error in confirm time player 4")
    }
    if(as.numeric(dat[p1_pos - 5]) != 1){
        stop("Round number error in confirm time player number");
    }
    if(as.numeric(dat[p2_pos - 5]) != 2){
        stop("Round number error in confirm time player number");
    }
    if(as.numeric(dat[p3_pos - 5]) != 3){
        stop("Round number error in confirm time player number");
    }
    if(as.numeric(dat[p4_pos - 5]) != 4){
        stop("Round number error in confirm time player number");
    }
    p1_confirm <- hms_sep(dat[p1_pos]);
    p2_confirm <- hms_sep(dat[p2_pos]);
    p3_confirm <- hms_sep(dat[p3_pos]);
    p4_confirm <- hms_sep(dat[p4_pos]);
    confirms   <- rbind(p1_confirm, p2_confirm, p3_confirm, p4_confirm);
    return(confirms);
}

hms_sep <- function(hms){
    num_time   <- strsplit(hms, split = ":")[[1]];
    hms_new    <- as.numeric(num_time);
    return(hms_new);
}


get_player_total_score <- function(dat, round){
    p1_pos <- which(dat == "Total")[(4 * (round - 1)) + 1] - 1;
    p2_pos <- which(dat == "Total")[(4 * (round - 1)) + 2] - 1;
    p3_pos <- which(dat == "Total")[(4 * (round - 1)) + 3] - 1;
    p4_pos <- which(dat == "Total")[(4 * (round - 1)) + 4] - 1;
    if(dat[p1_pos - 2] != "Round"){
        stop("Round number error in Total score");
    }
    if(dat[p2_pos - 2] != "Round"){
        stop("Round number error in Total score");
    }
    if(dat[p3_pos - 2] != "Round"){
        stop("Round number error in Total score");
    }
    if(dat[p4_pos - 2] != "Round"){
        stop("Round number error in Total score");
    }
    if(dat[p1_pos + 2] != "Score"){
        stop("Round number error in Total score");
    }
    if(dat[p2_pos + 2] != "Score"){
        stop("Round number error in Total score");
    }
    if(dat[p3_pos + 2] != "Score"){
        stop("Round number error in Total score");
    }
    if(dat[p4_pos + 2] != "Score"){
        stop("Round number error in Total score");
    }
    p1_score <- as.numeric(strsplit(dat[p1_pos], ","));
    p2_score <- as.numeric(strsplit(dat[p2_pos], ","));
    p3_score <- as.numeric(strsplit(dat[p3_pos], ","));
    p4_score <- as.numeric(strsplit(dat[p4_pos], ","));
    p_scores <- c(p1_score, p2_score, p3_score, p4_score);
    return(p_scores);
}

clean_five_commas <- function(dat){
    for(i in 1:length(dat)){
        if(is.character(dat[i]) == TRUE){
            fixed  <- strsplit(dat[i], split = ",,,,,")[[1]];
            dat[i] <- length(fixed);
            if(length(fixed) == 1){
                dat[i] <- fixed;
            }
        }
    }
    dat <- unlist(dat);
    return(dat);
}
