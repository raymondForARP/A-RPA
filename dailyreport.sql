-- I copy and paste this code once into the SQLite databse to streamline 
-- the process. There probably is a more efficient way of doing things? Also
-- datatypes need to be tweaked. Not every string needs to be a VARCHAR(50). 

--create schema for all  asset classes
CREATE TABLE equity (
	asset_class VARCHAR(50),
	date_id date,
	segment VARCHAR(50),
	securities_description VARCHAR(50),
	wkn VARCHAR(10),
	isin VARCHAR(50),
	currency VARCHAR(10),
	unit REAL, 
	book_price REAL, 
	exchange_rate REAL, 
	price REAL, 
	book_value_eur REAL,
	market_value_eur REAL,
	profit___loss_eur REAL, 
	x__in_portfolio REAL
);
CREATE TABLE ordinary (
	asset_class VARCHAR(50),
	date_id datatype,
	segment VARCHAR(50),
	securities_description VARCHAR(50),
	wkn VARCHAR(10),
	isin VARCHAR(50),
	sector VARCHAR(50),
	country VARCHAR(50), 
	currency VARCHAR(10),
	unit REAL, 
	book_price REAL, 
	exchange_rate REAL, 
	price REAL, 
	book_value_eur REAL,
	market_value_eur REAL,
	profit___loss_eur REAL, 
	x__in_portfolio REAL,
	borrowing_lending_position REAL, 
	other_positions REAL, 
	collateral_positions REAL, 
	margin_positions REAL, 
	corp_actions_positions REAL
);
CREATE TABLE extra (
	asset_class VARCHAR(50),
	date_id date,
	segment VARCHAR(50),
	securities_description VARCHAR(50),
	isin VARCHAR(50),
	currency VARCHAR(10),
	unit__nominal_value REAL, 
	book_price REAL, 
	market_value_eur REAL, 
	profit___loss_eur REAL, 
	x__in_portfolio REAL,
	price REAL
);
CREATE TABLE forwards (
	asset_class VARCHAR(50),
	date_id date,
	segment VARCHAR(50),
	type VARCHAR(50),
	counterparty___currency VARCHAR(50),
	nominal_value REAL,
	maturity VARCHAR(50),
	forward_rate REAL,
	price REAL,
	counter_currency VARCHAR(50),
	forward_value REAL,
	market_value REAL,
	exchange_rate REAL,
	receivable__liability_eur REAL,
	x__in_portfolio REAL,
	counter_party VARCHAR(50)
);
CREATE TABLE futures (
	asset_class VARCHAR(50),
	date_id date,
	segment VARCHAR(50),
	contract VARCHAR(50),
	wkn VARCHAR(10),
	isin VARCHAR(50),
	maturity VARCHAR(50),
	qty_of_contracts REAL, 
	contract_size REAL,
	type VARCHAR(50), 
	currency VARCHAR(50),
	book_price REAL,
	price REAL, 
	exchange_rate REAL, 
	book_value_eur REAL,
	market_value_eur REAL,
	receivable__liab__eur REAL, 
	x__in_portfolio REAL,
	counter_currency VARCHAR(50)
);
CREATE TABLE imargin (
	asset_class VARCHAR(50),
	date_id date,
	segment VARCHAR(50),
	currency VARCHAR(50),
	nominal_value REAL, 
	book_price REAL,
	price REAL,
	book_value_eur REAL, 
	market_value_eur REAL, 
	profit__loss_eur REAL, 
	x__in_portfolio REAL
);
CREATE TABLE options (
	asset_class VARCHAR(50),
	date_id date,
	segment VARCHAR(50),
	contract VARCHAR(50),
	wkn VARCHAR(10),
	isin VARCHAR(50),
	maturity VARCHAR(50),
	qty_of_contracts REAL, 
	contract_size REAL,
	type VARCHAR(50), 
	currency VARCHAR(50),
	strike_price REAL,
	book_price REAL,
	exchange_rate REAL, 
	price REAL, 
	book_value_eur_ REAL,
	market_value_eur__ REAL,
	receivable__liab__eur REAL, 
	x__in_portfolio REAL
);
CREATE TABLE overnight (
	asset_class VARCHAR(50),
	date_id date,
	segment VARCHAR(50),
	currency VARCHAR(50),
	nominal_value REAL, 
	interest_rate_in__ REAL, 
	exchange_rate REAL, 
	book_value_eur REAL,
	market_value_eur REAL,
	counterparty VARCHAR,
	x__in_portfolio REAL
);
                  
