# Where Is My Money ?

***wimm is still a work in progress***

wimm is my personnal budget and financial command line tool. It has the
following planned or implemented functionaly :
- [ ] Double entry accounting
- [ ] Single currency (this is a feature, a simpler tool is a better tool)
- [ ] Balance assertion
- [ ] Git friendly (all data stored as JSON/YAML file)
- [ ] Export various financial reports
  - [ ] Balance Sheet
  - [ ] Income Statement
  - [ ] Trial Balance
- [ ] Easy imports of bank CSV
  - [ ] Configurable via JSON file
  - [ ] Deduplication of transactions

wimm is not like other plain text accouting program in the sense that it does
not have a fancy text format for you to manually insert transaction. The idea is
that almost all my transactions are imported via my bank account. Also having
all data (including transactions) stored has JSON/YAML means you can use almost any
tool to analyse the data, since this is a widely supported format. 