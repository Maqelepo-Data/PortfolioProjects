

--select * from PortfolioProject..CovidDeaths
--order by 3,4

--select * from PortfolioProject..CovidVaccination
--order by 3,4

select * from PortfolioProject..CovidDeaths
where continent is not null
order by 3,4

select Location, date, total_cases, new_cases, total_deaths, population
from Portfolioproject..CovidDeaths
order by 1,2

-- looking at total cases vs total deaths
-- shows likelihood of dying of covid
select Location, date, convert(decimal,total_cases) as TotalCases, convert(decimal,total_deaths) as TotalDeaths, 
  (convert(decimal,total_deaths)/convert(decimal,total_cases))*100 as DeathPercentage
from Portfolioproject..CovidDeaths
where location like '%South Af%'
order by 1,2


--looking at tatal case vs population 
--show what percentage of population got covid
select Location, date, convert(decimal,population) as Population, convert(decimal,total_cases) as TotalCases, 
   (convert(decimal,total_cases)/convert(decimal,population))*100 as PopulationPercentage
from Portfolioproject..CovidDeaths
where location like '%South Af%'
order by 1,2

--looking for countries with highet infation rate per population
select Location, convert(decimal,population) as Population, convert(decimal,max(total_cases)) as HighetInfactionCount,
    max(convert(decimal,total_cases)/convert(decimal,population))*100 as PercentagePopulationInfected
from Portfolioproject..CovidDeaths
Group by Location, population
order by  PercentagePopulationInfected desc

--showing countrie with Highet deth count per population
--select cast( cast('997866.0' as numeric(9,2)) as int)

select Location,  max(cast(total_deaths as numeric)) as TotalDeathCount
from Portfolioproject..CovidDeaths
where continent is not null
Group by Location
order by TotalDeathCount desc


select Location,  max(cast(total_deaths as numeric)) as TotalDeathCount
from Portfolioproject..CovidDeaths
where  continent not like 'income%'
AND continent is null 
Group by Location
order by TotalDeathCount desc


--Deaths per continent
select continent,  max(cast(total_deaths as numeric)) as TotalDeathCount
from Portfolioproject..CovidDeaths
where continent is not null
Group by continent
order by TotalDeathCount desc


--Global numbers
select sum(convert(decimal,new_cases)) as TotalCases, sum(convert(decimal,new_deaths)) as TotalDeaths, 
  sum(convert(decimal,new_deaths))/sum(convert(decimal,new_cases))*100 as DeathPercentage
from Portfolioproject..CovidDeaths
where continent is not null
order by 1,2


--looking at total population vs vaccinations
--Select new_vaccinations AS new_vaccinations, CASE WHEN Isnumeric(new_vaccinations) = 1
--THEN CONVERT(DECIMAL(18,2),new_vaccinations) 
--ELSE 0 END AS new_vaccinations
--from PortfolioProject..CovidVaccination vac

--USE CTE
With PopulationVsVaccinated(continent, location, date, population, new_vaccinations, RollingPeopleVaccinated) as
(
	select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
	sum(convert(float, vac.new_vaccinations)) over (partition by dea.location order by 
	dea.location, dea.date) as RollingPeopleVaccinated
	from PortfolioProject..CovidVaccination vac
	join PortfolioProject..CovidDeaths dea
	on dea.location = vac.location
	and dea.date = vac.date
	where dea.continent is not null
	--order by 2,3
)
select *, (RollingPeopleVaccinated/population)*100
from PopulationVsVaccinated


--temp table
Drop table if exists #PercentagePopulationVaccinated
create table #PercentagePopulationVaccinated
(
	continent nvarchar(255),
	location nvarchar(255),
	date datetime,
	population numeric,
	new_vaccinated float,
	RollingPeopleVaccinated numeric
)
Insert into #PercentagePopulationVaccinated
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
	sum(convert(float, vac.new_vaccinations)) over (partition by dea.location order by 
	dea.location, dea.date) as RollingPeopleVaccinated
	from PortfolioProject..CovidVaccination vac
	join PortfolioProject..CovidDeaths dea
	on dea.location = vac.location
	and dea.date = vac.date
	where dea.continent is not null
	--order by 2,3

select *, (RollingPeopleVaccinated/population)*100 --as PercentagePopulationVaccinated
from #PercentagePopulationVaccinated


--creating a view to store data
Drop view if exists PercentagePopulationVaccinated
create view PercentagePopulationVaccinated as
select dea.continent, dea.location, dea.date, dea.population, vac.new_vaccinations,
	sum(convert(float, vac.new_vaccinations)) over (partition by dea.location order by 
	dea.location, dea.date) as RollingPeopleVaccinated
	from PortfolioProject..CovidVaccination vac
	join PortfolioProject..CovidDeaths dea
	on dea.location = vac.location
	and dea.date = vac.date
	where dea.continent is not null
	--order by 2,3
