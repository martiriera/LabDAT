
CREATE TABLE themes (id INTEGER PRIMARY KEY, leader TEXT, category TEXT, title TEXT, description TEXT);
CREATE TABLE questions (id INTEGER PRIMARY KEY, theme INTEGER, user TEXT, posted DATE, title TEXT, body TEXT);
CREATE TABLE answers (id INTEGER PRIMARY KEY, question INTEGER, user TEXT, posted DATE, body TEXT);

