SELECT
	p.match_id,
	TRY_CAST(b.commence_time AS datetime2) AT TIME ZONE 'UTC'
		AT TIME ZONE 'AUS Eastern Standard Time' AS commence_time_AEST,
	p.home_team,
	p.away_team,
	p.home_prediction,
	p.away_prediction,
	p.prediction,
	b.home_team_odds,
	b.away_team_odds,
	b.bookmaker,
	b.ev_bet,
	CASE 
		WHEN b.ev_bet = p.home_team THEN 'Home'
		WHEN b.ev_bet = p.away_team THEN 'Away'
		ELSE NULL
	END AS ev_homeaway,
	CASE
		WHEN b.ev_bet = p.home_team THEN b.home_team_odds
		WHEN b.ev_bet = p.away_team THEN b.away_team_odds
		ELSE NULL
	END AS bet_line,
	f.result,
	CASE
		WHEN f.result = 'H' THEN p.home_team
		WHEN f.result = 'A' THEN p.away_team
		ELSE NULL
	END AS result_team
FROM [out].[prediction_log] AS p
LEFT JOIN [out].[bets_log] AS b
	ON p.match_id = b.match_id
LEFT JOIN [feat].[feature_engineered_df] AS f
	ON p.match_id = f.match_id
ORDER BY match_id
