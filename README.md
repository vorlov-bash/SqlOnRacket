## Usage
```
> load("resources/<filename>")
> select * from resources/<filename>
> select col1,col2,col3,col1 from resources/<filename>
```
#### Example:
```
> select id_event,date_agenda,id_question from resources/plenary_register_mps-skl9.tsv
> select distinct col,id_fr,row,col from resources/map_zal-skl9.csv
```