from setuptools import setup, find_packages, find_namespace_packages

setup(
    name="gugugu-python-example",
    packages=[
        *find_packages("src"),
        *find_packages("gugugu-generated"),
        *find_namespace_packages(
            "gugugu-generated",
            include=["guguguexamples.*", "gugugu.*"],
        ),
    ],
    package_dir={
        "gugugu": "gugugu-generated/gugugu",
        "guguguexamples.definitions":
            "gugugu-generated/guguguexamples/definitions",
        "": "src",
    },
    include_package_data=True,
)
