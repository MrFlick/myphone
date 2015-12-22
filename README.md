# My Phone

An R package to extract SMS contents (text and attachments) from iPhone backup files (on a Mac)

## Usage

Here's some sample code 

```
devtools::install_github("mrflick/myphone");

backups <- list_backups();
if ( length(backups)>0) {
	bkfile <- backups[1];
	sms <- read_sms_data(bkfile);

	#count sent/recieved
	table(sms$Type)

	#distribution of sms lenth
	hist(nchar(sms$Text))
		
} else {
	print("No backups found!");
}
```
