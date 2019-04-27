-- I copy and paste this code once into the SQLite databse to streamline 
-- the process. There probably is a more efficient way of doing things? Also
-- datatypes need to be tweaked. Not every string needs to be a VARCHAR(50). 

-- create schema for all  asset classes

CREATE TABLE isin (
	ID VARCHAR(20) PRIMARY KEY,
	region VARCHAR(25),
	asset_type VARCHAR(25)
);

CREATE TABLE equity (
	equity_id INTEGER PRIMARY KEY AUTO_INCREMENT,
	asset_class VARCHAR(50),
	date_id TEXT,
	segment VARCHAR(50),
	securities_description VARCHAR(50),
	wkn VARCHAR(10),
	isin VARCHAR(50),
	currency VARCHAR(10),
	unit TEXT, 
	book_price TEXT, 
	exchange_rate TEXT, 
	price TEXT, 
	exchange_rate_1 TEXT, 
	book_value_eur TEXT,
	market_value_eur TEXT,
	profit___loss_eur TEXT, 
	x__in_portfolio TEXT
);
CREATE TABLE extra (
	extra_id INTEGER PRIMARY KEY AUTO_INCREMENT,
	asset_class VARCHAR(50),
	date_id TEXT,
	segment VARCHAR(50),
	securities_description VARCHAR(50),
	isin VARCHAR(50),
	currency VARCHAR(10),
	unit__nominal_value TEXT, 
	book_price TEXT, 
	market_value_eur TEXT, 
	profit___loss_eur TEXT, 
	x__in_portfolio TEXT,
	price TEXT
);
CREATE TABLE forwards (
	forwards_id INTEGER PRIMARY KEY AUTO_INCREMENT,
	asset_class VARCHAR(50),
	date_id TEXT,
	segment VARCHAR(50),
	type VARCHAR(50),
	counterparty___currency VARCHAR(50),
	nominal_value TEXT,
	maturity VARCHAR(50),
	forward_rate TEXT,
	price TEXT,
	counter_currency VARCHAR(50),
	forward_value TEXT,
	market_value TEXT,
	exchange_rate TEXT,
	receivable__liability_eur TEXT,
	x__in_portfolio TEXT,
	counter_party VARCHAR(50)
);
CREATE TABLE futures (
	futures_id INTEGER PRIMARY KEY AUTO_INCREMENT,
	asset_class VARCHAR(50),
	date_id TEXT,
	segment VARCHAR(50),
	contract VARCHAR(50),
	wkn VARCHAR(10),
	isin VARCHAR(50),
	maturity VARCHAR(50),
	qty_of_contracts TEXT, 
	contract_size TEXT,
	type VARCHAR(50), 
	type_1 VARCHAR(50),
	currency VARCHAR(50),
	book_price TEXT,
	price TEXT, 
	exchange_rate TEXT, 
	book_value_eur TEXT,
	market_value_eur TEXT,
	receivable__liab__eur TEXT, 
	x__in_portfolio TEXT,
	counter_currency VARCHAR(50),
	exchange_rate_1 TEXT
);
CREATE TABLE imargin (
	imargin_id INTEGER PRIMARY KEY AUTO_INCREMENT,
	asset_class VARCHAR(50),
	date_id TEXT,
	segment VARCHAR(50),
	currency VARCHAR(50),
	nominal_value TEXT, 
	book_price TEXT,
	price TEXT,
	book_value_eur TEXT, 
	market_value_eur TEXT, 
	profit__loss_eur TEXT, 
	x__in_portfolio TEXT
);
CREATE TABLE options (
	options_id INTEGER PRIMARY KEY AUTO_INCREMENT,
	asset_class VARCHAR(50),
	date_id TEXT,
	segment VARCHAR(50),
	contract VARCHAR(50),
	wkn VARCHAR(10),
	isin VARCHAR(50),
	maturity VARCHAR(50),
	qty_of_contracts TEXT, 
	contract_size TEXT,
	type VARCHAR(50), 
	type_1 VARCHAR(50),
	currency VARCHAR(50),
	strike_price TEXT,
	book_price TEXT,
	exchange_rate TEXT, 
	price TEXT, 
	exchange_rate_1 TEXT, 
	book_value_eur_ TEXT,
	market_value_eur__ TEXT,
	receivable__liab__eur TEXT, 
	x__in_portfolio TEXT
);
CREATE TABLE ordinary (
	ordinary_id INTEGER PRIMARY KEY AUTO_INCREMENT,
	asset_class VARCHAR(50),
	date_id TEXT,
	segment VARCHAR(50),
	securities_description VARCHAR(50),
	wkn VARCHAR(10),
	isin VARCHAR(50),
	sector VARCHAR(50),
	country VARCHAR(50), 
	currency VARCHAR(10),
	unit TEXT, 
	book_price TEXT, 
	exchange_rate TEXT, 
	price TEXT, 
	exchange_rate_1 TEXT, 
	book_value_eur TEXT,
	market_value_eur TEXT,
	profit___loss_eur TEXT, 
	x__in_portfolio TEXT,
	borrowing_lending_position TEXT, 
	other_positions TEXT, 
	collateral_positions TEXT, 
	margin_positions TEXT, 
	corp_actions_positions TEXT,
	other_positions_1 TEXT
);
CREATE TABLE overnight (
	overnight_id INTEGER PRIMARY KEY AUTO_INCREMENT,
	asset_class VARCHAR(50),
	date_id TEXT,
	segment VARCHAR(50),
	currency VARCHAR(50),
	nominal_value TEXT, 
	interest_rate_in__ TEXT, 
	exchange_rate TEXT, 
	book_value_eur TEXT,
	exchange_rate_1 TEXT, 
	market_value_eur TEXT,
	counterparty VARCHAR(50),
	x__in_portfolio TEXT
);
CREATE TABLE deposit ( 
	deposit_id INTEGER PRIMARY KEY AUTO_INCREMENT,
	asset_class VARCHAR(50),
	date_id TEXT,
	segment VARCHAR(50),
	currency VARCHAR(50),
	nominal_value TEXT,
	book_price TEXT,
	price TEXT,
	book_value_eur TEXT,
	market_value_eur TEXT,
	receivable__liability_eur TEXT,
	x__in_portfolio TEXT
);
                  
