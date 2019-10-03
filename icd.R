library(icd)

fd_map <- list(
	"Miscellaneous bacterial infections" = Reduce(union, list(
		expand_range("010","018",short_code=FALSE),
		expand_range("020","027",short_code=FALSE),
		expand_range("030","033",short_code=FALSE),
		expand_range("036","041",short_code=FALSE),
		expand_range("070","104",short_code=FALSE),
		expand_range("130","139",short_code=FALSE),
		expand_range("320","323",short_code=FALSE),
		expand_range("383","383",short_code=FALSE),
		expand_range("475","475",short_code=FALSE))),
	"Pneumonia" = expand_range("481","486",short_code=FALSE),
	"Urinary tract infections (UTI)" = Reduce(union, list(
		expand_range("590.1","590.1",short_code=FALSE),
		expand_range("590.2","590.2",short_code=FALSE),
		expand_range("590.8","590.8",short_code=FALSE),
		expand_range("590.9","590.9",short_code=FALSE),
		expand_range("595.0","595.0",short_code=FALSE),
		expand_range("595.9","595.9",short_code=FALSE),
		expand_range("599.0","599.0",short_code=FALSE))),
	"Acne" = expand_range("706.0","706.1",short_code=FALSE),
	"Gastrointestinal infections" = Reduce(union, list(
		expand_range("001","009",short_code=FALSE),
		expand_range("787","787",short_code=FALSE),
		expand_range("789","789",short_code=FALSE))),
	"Pharyngitis" = Reduce(union, list(
		expand_range("462","463",short_code=FALSE),
		expand_range("034","034",short_code=FALSE))),
	"Sinusitis" = Reduce(union, list(
		expand_range("461","461",short_code=FALSE),
		expand_range("473","473",short_code=FALSE))),
	"Skin cutaneous and mucosal infections" = Reduce(union, list(
		expand_range("680","686",short_code=FALSE),
		expand_range("035","035",short_code=FALSE),
		expand_range("110","111",short_code=FALSE),
		expand_range("704.8","704.8",short_code=FALSE),
		expand_range("728.0","728.0",short_code=FALSE),
		expand_range("611.0","611.0",short_code=FALSE),
		expand_range("771.5","771.5",short_code=FALSE),
		expand_range("728.86","728.86",short_code=FALSE),
		expand_range("380.0","380.1",short_code=FALSE))),
	"Suppurative otitis media" = expand_range("382","382",short_code=FALSE),
	"Asthma allergy" = Reduce(union, list(
		expand_range("493","493",short_code=FALSE),
		expand_range("477","477",short_code=FALSE),
		expand_range("995.3","995.3",short_code=FALSE))),
	"Brochitis bronchiolitis" = Reduce(union, list(
		expand_range("490","490",short_code=FALSE),
		expand_range("466","466",short_code=FALSE))),
	"Influenza" = expand_range("487","488",short_code=FALSE),
	"Miscellaneous other infections" = Reduce(union, list(
		expand_range("042","042",short_code=FALSE),
		expand_range("045","066",short_code=FALSE),
		expand_range("112","129",short_code=FALSE))),
	"Non-suppurative otitis media" = expand_range("381","381",short_code=FALSE),
	"Other gastrointestinal conditions" = expand_range("520","579",short_code=FALSE),
	"Other skin cutaneous and mucosal conditions" = setdiff(
		Reduce(union, list(
		expand_range("690","698",short_code=FALSE),
		expand_range("700","709",short_code=FALSE),
		expand_range("870","897",short_code=FALSE),
		expand_range("910","949",short_code=FALSE),
		expand_range("360","379",short_code=FALSE),
		expand_range("380","389",short_code=FALSE),
		expand_range("782","782",short_code=FALSE),
		expand_range("785.4","785.4",short_code=FALSE),
		expand_range("785.6","785.6",short_code=FALSE)))
		,
		Reduce(union, list(
		expand_range("380.0","380.1",short_code=FALSE),
		expand_range("706.0","706.1",short_code=FALSE),
		expand_range("704.8","704.8",short_code=FALSE),
		expand_range("381","383",short_code=FALSE)))
		),
	"Other genitourinary conditions" = setdiff(
		Reduce(union, list(
		expand_range("580","629",short_code=FALSE),
		expand_range("788.1","788.1",short_code=FALSE)))
		,
		Reduce(union, list(
		expand_range("590.1","590.1",short_code=FALSE),
		expand_range("590.2","590.2",short_code=FALSE),
		expand_range("590.8","590.8",short_code=FALSE),
		expand_range("590.9","590.9",short_code=FALSE),
		expand_range("595.0","595.0",short_code=FALSE),
		expand_range("595.9","595.9",short_code=FALSE),
		expand_range("599.0","599.0",short_code=FALSE),
		expand_range("611.0","611.0",short_code=FALSE)))
		),
	"Viral pneumonia" = expand_range("480","480",short_code=FALSE),
	"Viral upper respiratory infection (URI)" = Reduce(union, list(
		expand_range("460","460",short_code=FALSE),
		expand_range("464","464",short_code=FALSE),
		expand_range("465","465",short_code=FALSE),
		expand_range("786.2","786.2",short_code=FALSE))),
	"Other respiratory conditions" = setdiff(
		Reduce(union, list(
		expand_range("460","519",short_code=FALSE),
		expand_range("786.0","786.1",short_code=FALSE),
		expand_range("786.3","786.4",short_code=FALSE)))
		,
		Reduce(union, list(
		expand_range("010","018",short_code=FALSE),
		expand_range("020","027",short_code=FALSE),
		expand_range("030","033",short_code=FALSE),
		expand_range("036","041",short_code=FALSE),
		expand_range("070","104",short_code=FALSE),
		expand_range("130","139",short_code=FALSE),
		expand_range("320","323",short_code=FALSE),
		expand_range("383","383",short_code=FALSE),
		expand_range("475","475",short_code=FALSE),
		expand_range("481","486",short_code=FALSE),
		expand_range("590.1","590.1",short_code=FALSE),
		expand_range("590.2","590.2",short_code=FALSE),
		expand_range("590.8","590.8",short_code=FALSE),
		expand_range("590.9","590.9",short_code=FALSE),
		expand_range("595.0","595.0",short_code=FALSE),
		expand_range("595.9","595.9",short_code=FALSE),
		expand_range("599.0","599.0",short_code=FALSE),
		expand_range("706.0","706.1",short_code=FALSE),
		expand_range("001","009",short_code=FALSE),
		expand_range("787","787",short_code=FALSE),
		expand_range("789","789",short_code=FALSE),
		expand_range("462","463",short_code=FALSE),
		expand_range("034","034",short_code=FALSE),
		expand_range("461","461",short_code=FALSE),
		expand_range("473","473",short_code=FALSE),
		expand_range("680","686",short_code=FALSE),
		expand_range("035","035",short_code=FALSE),
		expand_range("110","111",short_code=FALSE),
		expand_range("704.8","704.8",short_code=FALSE),
		expand_range("728.0","728.0",short_code=FALSE),
		expand_range("611.0","611.0",short_code=FALSE),
		expand_range("771.5","771.5",short_code=FALSE),
		expand_range("728.86","728.86",short_code=FALSE),
		expand_range("380.0","380.1",short_code=FALSE),
		expand_range("382","382",short_code=FALSE),
		expand_range("493","493",short_code=FALSE),
		expand_range("477","477",short_code=FALSE),
		expand_range("995.3","995.3",short_code=FALSE),
		expand_range("490","490",short_code=FALSE),
		expand_range("466","466",short_code=FALSE),
		expand_range("487","488",short_code=FALSE),
		expand_range("042","042",short_code=FALSE),
		expand_range("045","066",short_code=FALSE),
		expand_range("112","129",short_code=FALSE),
		expand_range("381","381",short_code=FALSE),
		expand_range("520","579",short_code=FALSE),
		expand_range("690","698",short_code=FALSE),
		expand_range("700","709",short_code=FALSE),
		expand_range("870","897",short_code=FALSE),
		expand_range("910","949",short_code=FALSE),
		expand_range("360","379",short_code=FALSE),
		expand_range("380","389",short_code=FALSE),
		expand_range("782","782",short_code=FALSE),
		expand_range("785.4","785.4",short_code=FALSE),
		expand_range("785.6","785.6",short_code=FALSE),
		expand_range("580","629",short_code=FALSE),
		expand_range("788.1","788.1",short_code=FALSE),
		expand_range("480","480",short_code=FALSE),
		expand_range("460","460",short_code=FALSE),
		expand_range("464","464",short_code=FALSE),
		expand_range("465","465",short_code=FALSE),
		expand_range("786.2","786.2",short_code=FALSE)))
		)
	)

fd_map <- as.comorbidity_map(fd_map)
fd_map <- lapply(fd_map, decimal_to_short)

fd_df <- data.frame()
for(j in 1:length(fd_map)){
	fd_df <- rbind(fd_df, data.frame(COND=names(fd_map)[[j]], ICD=fd_map[[j]]))
}
fd_df <- rbind(fd_df, data.frame(COND="", ICD=""))
fd_df$COND = as.character(fd_df$COND)
fd_df$ICD = as.character(fd_df$ICD)