# Praćenje projekata

Ovo je Shiny aplikacija za unos i praćenje projekata.  
Omogućuje rad s zadacima te osnovne analize projekta pomoću PERT, CPM i Ganttograma.

Aplikacija je napravljena za potrebe kolegija.

---

## Što aplikacija radi

- unos projekata i zadataka
- postavljanje statusa zadataka
- definiranje prethodnika
- PERT analiza trajanja projekta
- CPM analiza i kritični put
- Ganttogram za pregled trajanja zadataka

---

## Pokretanje aplikacije

### Preduvjeti
- R
- RStudio

### Potrebni paketi
```r
install.packages(c("shiny", "ggplot2", "igraph", "DBI"))


### Struktura baze podataka

Aplikacija koristi tablicu `tasks` za pohranu podataka o projektima i zadacima.

```sql
CREATE TABLE dbo.tasks (
  id INT IDENTITY(1,1) PRIMARY KEY,
  project NVARCHAR(200) NOT NULL,
  task NVARCHAR(300) NOT NULL,
  status NVARCHAR(50) NOT NULL,
  start_date DATE NOT NULL,
  end_date DATE NOT NULL,
  a FLOAT NOT NULL,
  m FLOAT NOT NULL,
  b FLOAT NOT NULL,
  predecessor_id INT NULL
);



