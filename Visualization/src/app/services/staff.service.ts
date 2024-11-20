import { Injectable } from '@angular/core';
import { MessageService } from './message.service';
import { HttpClient, HttpHeaders } from '@angular/common/http';
import { catchError, map, Observable, of } from 'rxjs';
import { Specialization } from '../domain/specialization';
import { StaffWithId } from '../domain/staff-with-id';
import { Staff } from '../domain/staff';
import { StaffQueryParameters } from '../domain/staff-query-parameters';
import { IdPasser } from '../domain/IdPasser';
import { JwtHelperService } from '@auth0/angular-jwt';

@Injectable({
  providedIn: 'root'
})
export class StaffService {

  theServerURL = 'https://localhost:5001/api';
  constructor(private messageService: MessageService, private http: HttpClient) { }

  public getAllSpecializationsAvailable(): Observable<string[]> {
    const url = `${this.theServerURL}/Specializations`;
  
    return this.http.get<Specialization[]>(url)
                    .pipe(
                      map(data => data.map(spec => spec.denomination)),
                      catchError(this.handleError<string[]>('Get Specializations', []))
                    );
  }

  public createStaffProfile(firstName: string, lastName: string, phone: string, email: string, address: string, licenseNumber: string, specialization: string,functionDescrip:string) {
    const url = `${this.theServerURL}/Staff/Create-StaffProfile`;
    let staff: Staff = {
      licenseNumber: licenseNumber,
      firstName: firstName,
      lastName: lastName,
      phone: phone,
      email: email,
      address: address,
      function: functionDescrip,
      specializationId: specialization
    };

    this.http.post<Staff>(url, staff)
      .pipe(catchError(this.handleError<Staff>('Create staff profile')))
      .subscribe(data => {
        this.log(`Staff profile: ${data.email} was successfully created.`);
      });
  }

  deactivateStaffProfile(idStaff: string, token: string) {
    const url = `${this.theServerURL}/Staff/Deactivate-StaffProfile`;
    let idPasser: IdPasser = {
      id: idStaff
    };

    const headers = new HttpHeaders().set('Authorization', `Bearer ${token}`);

    this.http.put<{ message : string }>(url, idPasser, { headers})
      .pipe(catchError(this.handleError<{ message : string }>('Deactivate staff profile')))
      .subscribe(data => {
        this.log(`${data.message}`);
      });
  }

  getStaffByFilters(staffQueryParameters: StaffQueryParameters, token: string): Observable<StaffWithId[]> {
    const url = `${this.theServerURL}/Staff/Filtered-List`;
  
    // Setting up the headers
    const headers = new HttpHeaders().set('Authorization', `Bearer ${token}`);
  
    return this.http.post<StaffWithId[]>(url, staffQueryParameters, { headers}).pipe(
      catchError((error) => {
        // Handle 404 - No staff profiles found
        if (error.status === 404) {
          // Define fallback query parameters
          const queryParameters: StaffQueryParameters = {
            queryfilters: [
              {
                firstName: '',
                lastName: '',
                email: '',
                specialization: ''
              }
            ]
          };
  
          // Log the situation
          this.log('No staff profiles were found with the chosen criteria.');
  
          // Return the fallback request with the same headers and `withCredentials`
          return this.http.post<StaffWithId[]>(url, queryParameters, { headers, withCredentials: true });
        } else {
          // Handle other errors
          this.handleError<StaffWithId[]>('Get staff profile filtered list', error);
          return of([]); // Return an empty array to keep the app functional
        }
      })
    );
  }
  

getStaffById(id: string): Observable<StaffWithId> {
  const url = `${this.theServerURL}/Staff/${id}`;

  return this.http.get<StaffWithId>(url).pipe(
      catchError((error) => {
          this.handleError<StaffWithId>('Get staff profile', error);
          return of({} as StaffWithId);
      })
  );
}

  //------------------------/------------------------/------------------------
  private handleError<T>(operation = 'operation', result?: T) {
    return (error: any): Observable<T> => {

      if (error.status === 440) {
        this.log("Error: Login session expired.");
        return of(result as T);
      } else if (error.status === 401) {
        this.log("Error: Authentication is required.");
        return of(result as T);
      } else if (error.status === 403) {
        this.log("Error: Not allowed to access the feature.");
        return of(result as T);
      } else if(error.status === 200 ||error.status === 400 ){
        this.log(`${operation} failed: ${error.error}`);
      } else {
        this.log(`${operation} failed: an unexpected error occured.`);
      }
      return of(result as T);
    };
  }
  private log(message: string) {
    this.messageService.add(`${message}`);
  }
}
