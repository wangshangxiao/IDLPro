; Chapter04SetPeople.pro
PRO  Chapter04SetPeople
����COMMON SetPeopleInformation, MyName, MyAge, MySex, MyTime
����MyName =' '
����MyAge = 0B
����MySex =' '
����READ,  PROMPT = "������������", MyName
����READ,  PROMPT = "���������䣺", MyAge
����READ,  PROMPT = "�������Ա�Male �� Female����", MySex
����MyTime = SYSTIME()
END