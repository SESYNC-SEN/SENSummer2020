from sqlalchemy.orm import sessionmaker
from sqlalchemy import create_engine
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy import Column, Integer, Text, Numeric

Base = declarative_base()

class Food(Base):
    __tablename__ = 'food'
    
    id = Column(Integer, primary_key=True)
    name = Column(Text)
    sugar = Column(Numeric)
    
engine = create_engine('sqlite:///fruits.db')
Base.metadata.create_all(engine)
Session = sessionmaker(bind=engine)
