"""
    NetWeaver Health Data (NWHD) - Python Connector
    (c) 2023 by MDJoerg, mdjoerg@joomp.de
    Github: https://github.com/MDJoerg/sap_nwhd
    MIT License
    see examples at github, e.g. usage in jupyter notebooks
"""

# import other packages
import pandas as pd
import json

from pydeen.auth import AuthBasic
from pydeen.sap_abap import SAPAbapHttpBackend, SAPAbapODataConnector, SAPAbapODataRequest
from pydeen.types import Result, Factory
from pydeen.pandas import PandasResultDataframe
from pydeen.rest import HTTPRestConnector
from pydeen.menu import MenuSelection, UserInput


# Connector Class
class NWHDConnector:
    VERSION             = "20230413"
    GITHUB_REPO         = "https://github.com/MDJoerg/sap_nwhd"
    MAX_ROWS            = 10000

    API_PATH_DEFAULT    = "/nwhd_rest/v1"
    API_METHOD_SOURCES  = "/sources"
    API_METHOD_FDN_AV   = "/numeric_available"
    API_METHOD_FDN_TS   = "/numeric_timeseries"


    def __init__(self, name: str = None, url: str = None, client: str = "100", description: str = None, api_path: str = None):
        # define internal attributes
        self.systems: dict = {}
        self.name: str = None
        self.api_path: str = None
        self.max_rows = "1000"
        self.date_from = "20230101"
        self.date_to = "20231231"
        self.backend: SAPAbapHttpBackend
        self.connector: HTTPRestConnector
        self.source: str = None

        # print release info
        self.info()

        # set api path
        if api_path:
            self.api_path = api_path
        else:
            self.api_path = NWHDConnector.API_PATH_DEFAULT

        # configure abap system?
        if name is not None and url is not None:
            self.add_system(name, url, client, description)
            self.name = name
            if not self.prepare_system(name):
                print(f"preparing system {name} failed")
            else:
                sources = self.get_sources()
                print(f"\nAvailable sources:")
                for source in sources:
                    print(f"{source['SRC_GUID']} - {source['SOURCE_ID']:20} {source['SOURCE_DESCRIPTION']}")

    def set_date_interval(self, date_from:str, date_to:str):
        self.date_from = date_from
        self.date_to = date_to

    def set_max_rows(self, max_rows:int):
        if max_rows > 0:
            self.max_rows = max_rows
        else:
            self.max_rows = self.MAX_ROWS
            print("default max rows set: ", self.max_rows)

    def info(self):
        # init pydeen logger and write info messages
        Factory.get_datahub().info(f"NWHDConnector Version {NWHDConnector.VERSION}")
        Factory.get_datahub().info(f"See {NWHDConnector.GITHUB_REPO} for more details\n")

    def add_system(self, name: str, url:str, client:str, description:str):
        system = {"url": url, "description": description, "client": client}
        self.systems[name] = system


    def modify_properties(self):
        self.max_rows = UserInput("Max Rows", self.max_rows).get_input(empty_allowed=True)
        self.date_from = UserInput("Date from", self.date_from).get_input(empty_allowed=True)
        self.date_to = UserInput("Max Rows", self.date_to).get_input(empty_allowed=True)


    def load_json_file(self, filename: str) -> dict:
        try:
            json_string = ""
            with open(filename, "r") as file:
                for line in file:
                    if json_string == "":
                        json_string = line
                    else:
                        json_string += "\n" + line

            json_obj = json.loads(json_string)
            return json_obj
        except Exception as exc:
            print(f"Error while loading json from file: {type(exc)} - {exc}")
            return None

    def load_systems(self) -> bool:
        self.systems = self.load_json_file("nwhd_systems.json")
        if self.systems is not None and len(self.systems) > 0:
            return True
        else:
            return False

    def select_system(self) -> str:
        entries = {}
        for key in self.systems:
            system = self.systems[key]
            text = f"{key} - {system['description']} ({system['client']})"
            entries[key] = text
        action = MenuSelection("Select System", entries).show_menu()
        if action.is_cancel_entered():
            return None
        else:
            return action.get_selection()

    def prepare_system(self, system:str) -> bool:
        # prepare auth
        auth = AuthBasic()
        auth.set_menu_context(system)
        if not auth.load_config(auth.get_menu_filename()):
            auth.menu()

        # prepare system config
        config = self.systems[system]

        # init backend
        self.backend = SAPAbapHttpBackend(system, config["url"], config["client"], auth=auth)
        self.connector = HTTPRestConnector(self.backend)

        return True

    def get_sources(self) -> list:
        res_sources = self.connector.get_list(f"{self.api_path}{NWHDConnector.API_METHOD_SOURCES}")
        list_sources = res_sources.get_result_raw()
        return list_sources


    def select_source(self) -> str:
        list_sources = self.get_sources()
        if len(list_sources) == 0:
            return

        entries = {}
        for source in list_sources:
            src_guid = source["SRC_GUID"]
            src_id   = source["SOURCE_ID"]
            src_desc = source["SOURCE_DESCRIPTION"]
            entries[src_guid] = f"{src_id} - {src_desc}"

        action = MenuSelection("Select Source", entries).show_menu()
        if action.is_cancel_entered():
            return None
        else:
            return action.get_selection()

    def set_source(self, source: str, show_available: bool = True):
        self.source = source
        if self.source and show_available:
            available = self.get_available_numeric()
            if available is not None:
                print(f"\nSource {source} selected.")
                print("\nAvailable numeric values:")
                for value in available:
                    colname = f"{value['COLLECTOR']}:{value['CATEGORY']}:{value['FIELD']}"
                    print(f"{colname:50} = {value['COUNT']:10d} values")

    def get_available_numeric(self, source:str=None) -> list:
        # check source
        use_source = source
        if use_source is None:
            use_source = self.source
            if use_source is None:
                print("source not given")
                return None
        # get
        url = f"{self.api_path}{NWHDConnector.API_METHOD_FDN_AV}/{use_source}"
        print(url)
        res_available = self.connector.get_list(url)
        list_available = res_available.get_result_raw()
        return list_available


    def select_numeric(self, source:str, list_available:list=None) -> str:
        list_numeric = list_available
        if list_numeric is None:
            list_numeric = self.get_available_numeric(source)
        if len(list_numeric) == 0:
            return

        entries = {}
        for numeric in list_numeric:
            collector = numeric["COLLECTOR"]
            category  = numeric["CATEGORY"]
            field     = numeric["FIELD"]
            count     = numeric["COUNT"]
            id = f"{collector}:{category}:{field}"
            text = f"{collector} - {category} - {field} ({count})"
            entries[id] = text

        action = MenuSelection("Select Numeric Value", entries).show_menu()
        if action.is_cancel_entered():
            return None, list_numeric

        else:
            return action.get_selection(), list_numeric

    def get_numeric_values_as_result(self, source: str, collector:str, category:str, field:str, date_from:str=None, date_to:str=None, max_rows:int=10000) -> PandasResultDataframe:
        # prepare params
        params = {
            "from_date": self.date_from,
            "to_date": self.date_to,
            "max_rows": self.max_rows
        }
        if date_from:
            params["from_date"] = date_from
        if date_to:
            params["to_date"] = date_to
        if max_rows > 0:
            params["max_rows"] = str(max_rows)

        # call api
        url = f"{self.api_path}{NWHDConnector.API_METHOD_FDN_TS}/{source}/{collector}/{category}/{field}"
        res_values = self.connector.get_list(url, params)
        return res_values

    def get_numeric_values_as_dataframe(self, source: str, collector:str, category:str, field:str, date_from:str=None, date_to:str=None, max_rows:int=10000, prepare:bool=False, colname:str=None) -> pd.DataFrame:
        result = self.get_numeric_values_as_result(source, collector, category, field, date_from, date_to, max_rows)
        if result:
            df = result.get_result_as_pandas_df()
            if prepare:
                df['TIMESTAMP'] = pd.to_datetime(df['TIMESTAMP'], format='%Y-%m-%d %H:%M')
                df.set_index(pd.DatetimeIndex(df.TIMESTAMP), inplace=True)
                df.drop('TIMESTAMP', axis=1, inplace=True)
                if colname is None:
                    use_colname = f"{collector}:{category}:{field}"
                else:
                    use_colname = colname

                df.rename(columns={'VALUE': use_colname}, inplace=True)
            return df

    def get_df_numeric (self, colname: str) -> pd.DataFrame:
        # check source
        if self.source is None or ":" not in colname:
            print("wrong parameters to get prepared dataframe from current source")
            return
        # check column
        parts = colname.split(":")
        if len(parts) != 3:
            print("Full qualified column name required")
            return
        # select
        df = self.get_numeric_values_as_dataframe(self.source, collector=parts[0], category=parts[1], field=parts[2], date_from=self.date_from, date_to=self.date_to, max_rows=self.max_rows, prepare=True)
        return df

    def get_df_numeric_multiple(self, selected_values: list) -> pd.DataFrame:
        # check
        if selected_values is None or len(selected_values) == 0:
            return None
        # loop all values
        df_list = []
        for selected_value in selected_values:
            print(f"Select numeric value: {selected_value}")
            df = self.get_df_numeric(selected_value)
            df_list.append(df)

        # create df join
        df_join = pd.concat(df_list, axis=1)
        return df_join

    def get_numeric_values(self, source:str, selected_value:str) -> PandasResultDataframe:
        parts = selected_value.split(":")
        params = {
            "from_date": self.date_from,
            "to_date": self.date_to,
            "max_rows": self.max_rows
        }
        return self.get_numeric_values_as_result(source, parts[0], parts[1], parts[2])


    def select_result_action(self, subtitle:str=None) -> str:
        entries = {
            "menu": "Enter result menu",
            "dataframe": "Enter dataframe menu",
            "plot": "Plot"
        }
        action = MenuSelection("Select result action", entries, subtitle=subtitle).show_menu()
        if action.is_cancel_entered():
            return None
        else:
            return action.get_selection()

    def process_result_action(self, result:Result, action:str) -> bool:
        if action == "menu":
            result.menu()
            return True
        elif action == "dataframe":
            df = result.get_result_as_pandas_df()
            pd_result = PandasResultDataframe(action, df)
            pd_result.menu()
            return True
        elif action == "plot":
            df = result.get_result_as_pandas_df()
            df.plot()
            return True
        else:
            return False

    def process_result(self, result: Result) -> bool:
        subtitle = f"{result.get_count()} records"
        action = self.select_result_action(subtitle)
        if action is None:
            return False
        return self.process_result_action(result, action)

    def menu(self):
        # load systems config
        if not self.load_systems():
            print("missing systems config")
            return 1

        # select system
        system = self.select_system()
        if system is None:
            print("select system cancelled")
            return 0

        # prepare backend
        if not self.prepare_system(system):
            print("prepare system failed")
            return 2

        # prepare possible numeric values
        source = self.select_source()
        if source is None:
            return 0

        # modify default params
        self.modify_properties()

        # select numeric
        do_loop = True
        list_numeric = None
        while do_loop:
            numval, list_numeric = self.select_numeric(source, list_numeric)
            if numval is None:
                return 0

            result = self.get_numeric_values(source, numval)
            count_numeric = result.get_count()
            if count_numeric == 0:
                print("no results")
            else:
                processed = True
                while processed:
                    processed = self.process_result(result)

# run the script in menu mode
if __name__ == "__main__":
    exit(NWHDConnector().menu())


